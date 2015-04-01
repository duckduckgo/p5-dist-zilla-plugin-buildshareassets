package Dist::Zilla::Plugin::BuildShareAssets;
# ABSTRACT: Compile instant answer templates and create master javascript file

use Moose;
use namespace::autoclean;
with 'Dist::Zilla::Role::FileGatherer';

use Dist::Zilla::File::OnDisk;

use Time::HiRes qw(tv_interval gettimeofday);
use File::Temp qw(tempdir tempfile);
use List::MoreUtils 'part';
use POE qw(Wheel::Run Filter::Reference);
use IO::All;
use JSON::XS;

# Open our files in utf8
use open ':utf8';

use strict;
no warnings 'uninitialized';

has concurrent_builds => (
    is => 'ro',
    default => 2 
);

has metadata_path => (
    is => 'rw',
    default => 'share/{ia_type}/meta/metadata.json'
);

has id_map => (
    is => 'ro',
    lazy => 1,
    builder => 'load_metadata'
);

# The instant answer type we're processing. When set it will also set the
# metadata_path
has ia_type => (
    is => 'rw',
    trigger => \&_init_metadata_path
);

# Instant answers we don't want to build. Space-separated string
has exclude => (
    is   => 'ro',
    default => 'spice_template' 
);

sub _init_metadata_path {
    my ($self, $new) = @_;
    
    my $mp = $self->metadata_path;
    
    # If metadata_path was set externally, this will fail
    if($mp =~ s/{ia_type}/$new/){
        $self->metadata_path($mp);
    }

    # Let's check that it exists now
    $self->log_fatal(['Metadata path %s does not exists!', $mp]) unless -f $mp;
}

# The required plugin sub
sub gather_files {
    my $self = shift;

    POE::Session->create(
        package_states => [
            $self => [qw(_start _do_builds _build_result _build_err _build_done _fk _stop _default)],
        ],
        args => [$self]
    );
    POE::Kernel->run;
}

sub _start {
    my ($k, $h, $s) = @_[KERNEL, HEAP, ARG0];
    
    # Make STDOUT unbuffered
    select((select(STDOUT), $|=1)[0]);

    $h->{self} = $s;

    # All js/handlebars files in share
    my @files = io('share')->filter(sub{/\.(?:js|handlebars)$/o})->All_Files;
    unless(@files){
        $s->log_debug(['No js/handlebars files found to process']);
        return;
    }

    my ($exclude, %dirs, $found_ia_type) = ($s->exclude);
    for my $f (@files){
        unless($f =~ m{^(share/(goodie|spice|fathead|longtail)/(.+))/}o){
            $s->log_debug(['Skipping unrecongized file path: %s', $f]);
            next;
        }
        my ($dir, $ia_type, $ia_name) = ($1, $2, $3);
        next if exists $dirs{$dir};

        $ia_name =~ s{/}{_}og;

        # Skip excluded instant answers 
        if($exclude =~ /\b$ia_name\b/){
            $s->log_debug(['Detected excluded ia: %s', $ia_name]);
            next;
        }
        
        # This will give us unique IA directories
        $dirs{$dir} = [$dir, $ia_name];

        # Set the global instant answer type
        $s->ia_type($ia_type) unless $found_ia_type++;
    }
    $s->log_fatal(['No instant answer type found from share directory file paths']) unless $found_ia_type;

    # Divide directories into max_build number of queues
    my ($max_builds, $i) = ($s->concurrent_builds, 0);
    $h->{queues} = [part { $i++ % $max_builds } values %dirs];

    $s->log(['Building instant answers for %s', $s->ia_type]);    
    #$s->log_debug(['MAX BUILDS: %d', $max_builds]);
    $h->{t0} = [gettimeofday];
    $k->yield('_do_builds');
}

# Forks off max_build number of builders to do the work
sub _do_builds {
    my ($k, $h) = @_[KERNEL, HEAP];

    my $s = $h->{self};

    # This should remove the entire directory on a clean exit
    $h->{tmpdir} = eval { tempdir('dzilXXXX', TMPDIR => 1, CLEANUP => 1); }
        or $s->log_fatal(["Failed to create tmpdir: $@"]);

    for my $q (@{$h->{queues}}){    
        my $b = POE::Wheel::Run->new(
            Program      => \&build_ia,
            ProgramArgs  => [$q],
            CloseOnCall  => 1,
            NoSetSid     => 1,
            StdoutEvent  => '_build_result',
            StderrEvent  => '_build_err',
            CloseEvent   => '_build_done',
            StdioFilter  => POE::Filter::Reference->new,
            StderrFilter => POE::Filter::Line->new    
        );
        #$s->log_debug(['Adding pid %d', $b->PID]);
        $h->{builders}->{$b->ID} = $b;
        $k->sig_child($b->PID, '_fk');
    }
}

# Handle the normal result from a builder
sub _build_result {
    my ($h, $res) = @_[HEAP, ARG0];

    my ($tmp, $dist) = @$res;

    if($dist){
        # build file from tmpfile content
        my $f = Dist::Zilla::File::OnDisk->new(name => $tmp);
        # rename to distro file
        $f->name($dist);
        $h->{self}->add_file($f);
    }
    elsif($tmp =~ /FATAL/){
        $h->{self}->log_fatal([$tmp]);
    }
    else{
        # Shouldn't get here so let's make sure we see the message
        $h->{self}->log(['Unknown result returned from build process: %s', $tmp]);
    }
}

# Error output from builders
sub _build_err {
    my ($h, $e) = @_[HEAP, ARG0];

    $h->{self}->log_debug([$e]);
}

# Cleanup the heap after a builder has exited
sub _build_done {
    my ($h, $id) = @_[HEAP, ARG0];

    delete $h->{builders}->{$id};
    #$h->{self}->log_debug(['Removed builder %d. %d builders still running', $id, scalar(keys %{$h->{builders}})]);
}

# When the session has no more work and is about to exit
sub _stop {
    my $h = $_[HEAP];

    $h->{self}->log_debug(['Total elapsed build time: %ss', tv_interval($h->{t0}, [gettimeofday])]);
}

# Take care of any errant signals
sub _fk { $_[HEAP]->{self}->log_debug(['Got signal from pid %d', $_[ARG1]]);  $_[KERNEL]->sig_handled; }

# Mostly for debug to alert us of unhandled events
sub _default { 
    my ($h, $e) = @_[HEAP, ARG0];

    $h->{self}->log_debug(["We don't explicitly handle event", $e]);
}

sub load_metadata {
    my $self = shift;

    my $id_map = eval { decode_json(io($self->metadata_path)->slurp); }
        or $self->log_fatal(["Error reading metadata file: $@"]);

    return $id_map;
}

# The main sub that the builders run
sub build_ia {
    my $queue = shift;

    # $poe_kernel is available to all children. So we can pull
	# the heap from it and save having to pass certain globals in.
    my $h = $poe_kernel->get_active_session->get_heap;
    my $s = $h->{self};
    my ($id_map, $ia_type, $tmpdir) = ($s->id_map, $s->ia_type, $h->{tmpdir});

    #warn 'This builder has ', scalar(@$queue), " instant answers to process\n";

    # single filter for all IAs to message parent
    my $filter = POE::Filter::Reference->new;

    #my ($md_build_time, $hb_build_time, $js_build_time);
    for my $ia (@$queue){
        my ($dir, $ia_name) = @$ia;
        warn "\tBuilder $$ processing $ia_name\n";

        # We create multiple temp files using the same pattern
        my $get_temp_file = sub {
            my ($fh, $path);
            eval {
                ($fh, $path) = tempfile("${ia_name}XXXX", $tmpdir);
            } or
            do {
                fatal_error("Failed to create temp file: $@\n");
            };
            return [$fh, $path];
        };

        # (zt) The order that files are added to @js_files for later
        # concatenation *might* make a difference.
        my @js;
    
        #my $t0 = [gettimeofday];
        if(my $iax = $id_map->{$ia_name}) {
            my $f = build_metadata($ia_name, $iax, $get_temp_file);
            push @js, $f;
        }
        else{
            # Missing metadata is a warning for now. We probably should skip it but
            # AutoModuleShareDirs adds an entry in Makefile.PL for every directory
            # by default
            warn "WARNING - No metadata found for $ia_name";
        }
        #my $t1 = tv_interval($t0, [gettimeofday]);
        #$md_build_time += $t1;
        #warn "\tMetatadata build for $ia_name took ${t1}s\n";

        #$t0 = [gettimeofday];
        if(my @hbs = <$dir/*.handlebars>) {
            my $f = build_handlebars($ia_name, \@hbs, $get_temp_file);
            push @js, $f;
        }
        #$t1 = tv_interval($t0, [gettimeofday]);
        #$hb_build_time += $t1;
        #warn "\tHandlebars build for $ia_name took ${t1}s\n";
    
        my $ia_js = "$dir/$ia_name.js";
        push @js, $ia_js if -f $ia_js;

        #$t0 = [gettimeofday];
        if(@js){
            my $js = build_js($ia_name, \@js, $get_temp_file);
            # We pass the current tmp file name and the distribution
            # file name back to the parent to be added
            my $result = $filter->put([[$js, "$dir/$ia_name." . $ia_type . '.js']]);
            print STDOUT @$result;
        }
        else{
            # For potential subdirectories that don't really represent IAs.
            warn "No javascript or handlebars found for $ia_name...skipping";
        }

        #$t1 = tv_interval($t0, [gettimeofday]);
        #$js_build_time += $t1;
        #warn "\tFinal javascript build for $ia_name took ${t1}s\n";
    }

    #warn "Builder $$ spent its time working on the following:\n", 
    #    "\tmetadata total buildtime: ${md_build_time}s\n",
    #    "\thandlebars total buildtime: ${hb_build_time}s\n",
    #    "\tjavascript total buildtime: ${js_build_time}s\n";
}

sub build_metadata {
    my ($fn, $iax, $get_temp_file) = @_;

    # create a metadata object for the front end
    my %ia;
    for my $m (qw(id name attribution description topic)){
        unless(exists $iax->{$m}){
            fatal_error("missing value for $m");
        }
        $ia{$m} = $iax->{$m};
    }
    $ia{url} = 'https://duck.co/ia/view/' . $ia{id};

    my $metadata = eval{ encode_json(\%ia) } or fatal_error("Failed to encode json: $@");
    my ($fh, $metatmp) = @{ $get_temp_file->() };
    print $fh ";DDH.$fn = DDH.$fn || {};\nDDH.$fn.meta = $metadata;";

    return $metatmp;
}

sub build_handlebars {
    my ($fn, $hbs, $get_temp_file) = @_;

    my $hbc;
    # skip zero length handlebars files
    my @inc_hb;
    for my $hb (@$hbs) {
        if (-z $hb) {
            warn "Skipping zero length file $hb";
            #unlink $hb; # (zt) should we really be deleting what might be stubs?
        }
        else{ push @inc_hb, $hb; }
    }
    if(@inc_hb){
        $hbc = $get_temp_file->()->[1];
        if(system "handlebars -m -n DDH.$fn @inc_hb -f $hbc"){
            fatal_error("Failed to run handlebars: $?");
        }

    }
    return $hbc;
}

sub build_js {
    my ($ia_name, $jsf, $get_temp_file) = @_;
    
    my $js = $get_temp_file->()->[1];
    if(system "uglifyjs @$jsf -o $js"){
        fatal_error("Failed to build js for $ia_name");
    }
    return $js;
}

sub fatal_error {
    my $e = shift;

    my $f = POE::Filter::Reference->new;
    my $fe = $f->put([["FATAL - $e"]]);
    print STDOUT @$fe;
}

__PACKAGE__->meta->make_immutable;

1;

__END__


=encoding utf8 

=head1 NAME

Dist::Zilla::Plugin::BuildShareAssets - compile handlebars templates
and create super javascript file for production

=head1 SYNOPSIS

Performs the following steps in a zeroclickinfo-* repository:

=over

=item 1

Locates directories in share/ containing F<*.js> or F<*.handlebars> files

=item 2 

Builds metadata json for instant answer

=item 3

Compiles handlebars files, if any

=item 4

Concatenates above files and I<[ia_name].js>, if it exists,
into I<[ia_name].[ia_type].js>.

=back

The resulting file is then added to the distriubtion. Files used in the
creation of the file should probably be excluded via
L<Dist::Zilla::Plugin::PruneFiles>.

To activate the plugin, add the following to F<dist.ini>:

    [BuildShareAssets]


=head1 ATTRIBUTES

=head2 concurrent_builds

The number of concurrent builders to spawn (default: 2)

=head2 metadata_path

Path to the metadata.json file (default: share/{ia_type}/meta/metadata.json)

=head2 exclude

Exclude instant answers from being built, e.g. spice_template. Separate
multiple instant answers with spaces.

=head1 NON-PERL DEPENDENCIES

Uses handlebars and uglifyjs command-line utilities and expects them
to be in its path.

    L<handlebars|https://www.npmjs.com/package/handlebars>
    L<uglify-js|https://www.npmjs.com/package/uglify-js>

=head1 CONTRIBUTING

To browse the repository, submit issues, or bug fixes, please visit
the github repository:

=over 4

L<https://github.com/duckduckgo/p5-dist-zilla-plugin-buildshareassets>

=back

=head1 AUTHOR

Zach Thompson <zach@duckduckgo.com>

=cut
