# Databrary

## Installation

There is an (partial and unused) [Docker file](Dockerfile) that lays out most of the installation steps.

### Requirements

- ffmpeg >= 2.4, < 3.0, both shared libraries and ffmpeg, built against:
  - lame >= 3.99.5
  - fdk-aac >= 0.1.4
  - x264 git stable
  - Recommended configure flags: --enable-shared --disable-runtime-cpudetect --enable-gpl --enable-version3 --enable-nonfree --enable-libx264 --enable-libfdk-aac --enable-libmp3lame
- PostgreSQL >= 9.3
  - Requires an additional plugin: make -C dbrary/pgranges install
- node and npm
- cracklib with shared library
- solr >= 5.3
- GHC == 7.10
- cabal-install >= 1.22.6
- blaze-markup with patches from https://github.com/dylex/blaze-markup (to enable HTML administration interfaces)

### Configuration

Copy [example.conf](example.conf) to databrary.conf and edit to taste.
Make sure to follow the instructions in that file.

### Building

Build and run using:

    ./dev

This script takes a few optional flags:

- -n: Do not run the application
- -f: Force a reconfigure
- -p: Do a production-mode build (implies -n)
- -i: Install after build (implies -n)

## Layout

* [Master database schema](schema.sql)

### Object storage

The default configuration stores objects under store/ and cache/, which will
both need to be created or changed to other existing directories in local.conf.

## Usage

### Code Style

#### HTML

* Always use double-quotes.
* Indent with two spaces.
* IDs and classes are dash-spearated, not under_score, not camelCase.
* Lowercase everything.
* App links should not have target or title.
* External links should always have target and title.

### Stylus

* Indent with two spaces.
* Keep template-specific styles with template.
* Never use #ids to style.
* Only use elements for global styles.
* Put a class on every element you style.
* Don't use on inheritance.
* Name classes based on context.
* Don't share styles between templates.
* Always use single quotes.
* Use dash-class-names, never under_score, never camelCase.
* If you ever have to override a class-based style, you are doing something wrong.

#### JS

* Always use single quotes.
* Indent with two spaces.
* Use === as often as possible.

#### AngularJS

* Bind everything related to a form to the form object.
* Store form data in form.data, preferably in the structure you'd send to the server.
* Make as few new scopes as possible.

## License

Copyright (C) 2013-2016 New York University

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
