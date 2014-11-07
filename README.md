# emacs.d

This is my personal .emacs.d directory.

Emacs version: GNU emacs 24.3

Emacs using this repository is most often run by the command:
    emacsclient -c -a ""
which starts an emacs server if not running already, and connects to it on a new
frame.  It is of course possible to use it running regular emacs instead of
a client, but it is customized to work best with the above command.

This repository is customized for GNU/Linux and Windows, but it could, with some
customization, be used on other operating systems (e.g. Mac OS X) .

No third-party packages are included.  Most of them are downloaded from online
ELPA package archives automatically; some of them *must be downloaded manually*,
as there is no recent version of them in online package archives.  For more
information, see [packages/manual/README.md](packages/manual/README.md).

## License

Copyright 2014  Bence Kalmar <bkalmar1996@gmail.com>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
