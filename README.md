# Databrary

http://databrary.github.io/databrary

## Layout

* [Master database schema](schema.sql)

## Installation

Requirements:
- PostgreSQL >= 9.3
- ffmpeg >= 2.4 (not libav) with shared libraries
- cracklib with shared library
- GHC == 7.10

### New Haskell-Databrary Installation steps

    git clone https://github.com/databrary/databrary.git

Copy and edit example.conf to databrary.conf.  Build and run using:

    ./dev

### Postgres

You must manually install pgranges after each postgres upgrade:

    make -C dbrary/pgranges install

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
