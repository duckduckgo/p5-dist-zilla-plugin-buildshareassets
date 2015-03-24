# Dist::Zilla::Plugin::BuildShareAssets

## Synopsis

Creates a single javascript file for each instant answer by performing the following steps from the root of a zeroclickinfo-* repository:

1.  Locates directories in share/ containing *.js or *.handlebars files.
2.  Builds metadata json for instant answer.
3.  Compiles handlebars files, if any.
4.  Concatenates above files and [ia_name].js, if it exists, into [ia_name].[ia_type].js.
5.  Adds new js files to distribution.

Files concatenated together should probably be excluded from the distribution via Dist::Zilla::Plugin::PruneFile in dist.ini.

The plugin can be activated by adding the following to **dist.ini**:

    [BuildShareAssets]

## Attributes

There are few attributes that can be set in dist.ini.

- **max_concurrent_builds**: The number of concurrent builders to spawn (default: 2)
- **metadata_path**: Path to the metadata.json file (default: share/{ia_type}/meta/metadata.json)
- **exclude**: Exclude instant answers from being built, e.g. spice_template. Separate multiple instant answers with spaces.

## Non-Perl Dependencies

Uses command-line utilites, i.e. handlebars and uglifyjs, from the following packages and expects them to be in its path:

+ [handlebars](https://www.npmjs.com/package/handlebars)
+ [uglify-js](https://www.npmjs.com/package/uglify-js)
