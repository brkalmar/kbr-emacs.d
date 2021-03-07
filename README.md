Personal `.emacs.d` directory for GNU emacs >= 26.3.

Optimized for running emacs as a daemon with multiple clients.
The following command starts the emacs daemon if not running already, and connects to it on a new frame:

```shell
emacsclient -c -a "" [FILE]...
```

## Operating system

The repository is configured for GNU/Linux only.

## Third-party packages

No third-party packages are included.
Most of them are downloaded from online ELPA package archives automatically.

Some of them *must be downloaded manually*, as there is no recent version of them in online package archives.
See [packages/manual/README.md](packages/manual/README.md).

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
