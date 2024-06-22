GNU Emacs Installation Guide
Copyright (C) 1992, 1994, 1996, 1997, 2000, 2001, 2002, 2003, 2004,
2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.
See the end of the file for license conditions.


This file contains general information.  For more specific information
for the Windows, GNUstep/Mac OS X, and MS-DOS ports, also see the files
nt/INSTALL nextstep/INSTALL, and msdos/INSTALL.  For information
specific to building from a CVS checkout (rather than a release), see
the file INSTALL.CVS.


BASIC INSTALLATION

The simplest way to build Emacs is to use the `configure' shell script
which attempts to guess correct values for various system-dependent
variables and features and find the directories where various system
headers and libraries are kept.  It then creates a `Makefile' in each
subdirectory and a `config.h' file containing system-dependent
definitions.  Running the `make' utility then builds the package for
your system.

Here's the procedure to build Emacs using `configure' on systems which
are supported by it.  If this simplified procedure fails, or if you
are using a platform such as MS-Windows, where `configure' script
doesn't work, you might need to use various non-default options, and
maybe perform some of the steps manually.  The more detailed
description in the rest of the sections of this guide will help you do
that, so please refer to them if the simple procedure does not work.

  1. Make sure your system has at least 120 MB of free disk space.

  2a. `cd' to the directory where you unpacked Emacs and invoke the
      `configure' script:

		 ./configure

  2b. Alternatively, create a separate directory, outside the source
      directory, where you want to build Emacs, and invoke `configure'
      from there:

		 SOURCE-DIR/configure

      where SOURCE-DIR is the top-level Emacs source directory.  This
      may not work unless you use GNU make.

  3. When `configure' finishes, it prints several lines of details
     about the system configuration.  Read those details carefully
     looking for anything suspicious, such as wrong CPU and operating
     system names, wrong places for headers or libraries, missing
     libraries that you know are installed on your system, etc.

     If you find anything wrong, you will have to pass to `configure'
     explicit machine configuration name, and one or more options
     which tell it where to find various headers and libraries; refer
     to DETAILED BUILDING AND INSTALLATION section below.

     If `configure' didn't find some image support libraries, such as
     Xpm, jpeg, etc., and you want to use them refer to the subsection
     "Image support libraries", below.

     If the details printed by `configure' don't make any sense to
     you, assume that `configure' did its job and proceed.

  4. If you need to run the `configure' script more than once (e.g.,
     with some non-default options), always clean the source
     directories before running `configure' again:

		make distclean
		./configure

  5. Invoke the `make' program:

		 make

  6. If `make' succeeds, it will build an executable program `emacs'
     in the `src' directory.  You can try this program, to make sure
     it works:

		 src/emacs -q

  7. Assuming that the program `src/emacs' starts and displays its
     opening screen, you can install the program and its auxiliary
     files into their installation directories:

		 make install

  You are now ready to use Emacs.  If you wish to conserve disk space,
  you may remove the program binaries and object files from the
  directory where you built Emacs:

		 make clean

  You can also save some space by compressing (with `gzip') Info files
  and installed Lisp source (.el) files which have corresponding .elc
  versions.


ADDITIONAL DISTRIBUTION FILES

* Complex Text Layout support libraries

Emacs needs the optional libraries "m17n-db", "libm17n-flt", "libotf"
to correctly display such complex scripts as Indic and Khmer.
On some systems, particularly GNU/Linux, these libraries may be
already present or available as additional packages.  Note that if
there is a separate `dev' or `devel' package, for use at compilation
time rather than run time, you will need that as well as the
corresponding run time package; typically the dev package will contain
header files and a library archive.  Otherwise, you can download and
build libraries from sources.

The sources of these libraries are available by anonymous CVS from
cvs.m17n.org.

    % cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/m17n login
    % cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/m17n co m17n-db
    % cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/m17n co m17n-lib
    % cvs -d :pserver:anonymous@cvs.m17n.org:/cvs/m17n co libotf

For m17n-lib, if you have problems with making the whole package
because you lack some other packages on which m17n-lib depends, try to
configure it with the option "--without-gui".

* intlfonts-VERSION.tar.gz

The intlfonts distribution contains X11 fonts in various encodings
that Emacs can use to display international characters.  If you see a
non-ASCII character appear as a hollow box, that means you don't have
a font for it.  You might find one in the intlfonts distribution.  If
you do have a font for a non-ASCII character, but some characters
don't look right, or appear improperly aligned, a font from the
intlfonts distribution might look better.

The fonts in the intlfonts distribution are also used by the ps-print
package for printing international characters.  The file
lisp/ps-mule.el defines the *.bdf font files required for printing
each character set.

The intlfonts distribution contains its own installation instructions,
in the intlfonts/README file.

* Image support libraries

Emacs needs optional libraries to be able to display images (with the
exception of PBM and XBM images whose support is built-in).

On some systems, particularly on GNU/Linux, these libraries may
already be present or available as additional packages.  Note that if
there is a separate `dev' or `devel' package, for use at compilation
time rather than run time, you will need that as well as the
corresponding run time package; typically the dev package will
contain header files and a library archive.  Otherwise, you can
download and build libraries from sources.  None of them are vital for
running Emacs; however, note that Emacs will not be able to use
colored icons in the toolbar if XPM support is not compiled in.

Here's the list of these optional libraries, and the URLs where they
can be found:

  . libXaw3d for fancy 3D-style
      scroll bars:    ftp://ftp.x.org/contrib/widgets/Xaw3d/
  . libxpm for XPM:   ftp://ftp.x.org/contrib/libraries/
		      Get version 3.4k or later, which lets Emacs
		      use its own color allocation functions.
  . libpng for PNG:   ftp://ftp.simplesystems.org/pub/libpng/png/
  . libz (for PNG):   http://www.zlib.net/
  . libjpeg for JPEG: ftp://ftp.uu.net/graphics/jpeg/
                      Get version 6b -- 6a is reported to fail in
                      Emacs.
  . libtiff for TIFF: http://www.libtiff.org/
  . libgif for GIF:   http://sourceforge.net/projects/giflib/

Emacs will configure itself to build with these libraries if the
`configure' script finds them on your system, unless you supply the
appropriate --without-LIB option.  In some cases, older versions of
these libraries won't work because some routines are missing, and
configure should avoid such old versions.  If that happens, use the
--without-LIB options to `configure'.  See below for more details.

* Extra fonts

The Emacs distribution does not include fonts and does not install
them.  You must do that yourself.

Emacs running on the GNU system supports both X fonts and local fonts
(i.e. the fonts managed by the fontconfig library).

For `Unicode' (ISO 10646) X fonts, see
<URL:http://czyborra.com/unifont/> (packaged in Debian),
<URL:http://openlab.ring.gr.jp/efont/> (packaged in Debian).  (In
recent Debian versions, there is an extensive `misc-fixed' iso10646-1
in the default X installation.)  Perhaps also see
<URL:http://www.cl.cam.ac.uk/%7Emgk25/ucs-fonts.html>.

<URL:http://czyborra.com/charsets/> has basic fonts for Emacs's
ISO-8859 charsets.

XFree86 release 4 (from <URL:ftp://ftp.xfree86.org/pub/XFree86/> and mirrors)
contains font support for most, if not all, of the charsets that Emacs
currently supports, including iso10646-1 encoded fonts for use with
the mule-unicode charsets.  The font files should also be usable with
older X releases.  Note that XFree 4 contains many iso10646-1 fonts
with minimal character repertoires, which can cause problems -- see
etc/PROBLEMS.

BDF Unicode fonts etl-unicode.tar.gz are available from
<URL:ftp://ftp.x.org/contrib/fonts/> and
<URL:ftp://ftp.xfree86.org/pub/mirror/X.Org/contrib/fonts/>.  These
fonts can also be used by ps-print and ps-mule to print Unicode
characters.

Finally, the Web page <URL:http://www.gnu.org/software/freefont/>
lists a large number of free Unicode fonts.

* GNU/Linux development packages

Many GNU/Linux systems do not come with development packages by
default; they just include the files that you need to run Emacs, but
not those you need to compile it.  For example, to compile Emacs with
X11 support, you may need to install the special `X11 development'
package.  For example, in April 2003, the package names to install
were `XFree86-devel' and `Xaw3d-devel' on Red Hat.  On Debian, the
packages necessary to build the installed version should be
sufficient; they can be installed using `apt-get build-dep emacs21' in
Debian 3 and above.


DETAILED BUILDING AND INSTALLATION:

(This is for a Unix or Unix-like system.  For MS-DOS and Windows 3.X,
see msdos/INSTALL.  For Windows 9X, Windows ME, Windows NT, Windows
2000, Windows XP/2003, and Windows Vista/2008, see the file
nt/INSTALL.  For GNUstep and Mac OS X, see nextstep/INSTALL.)

1) Make sure your system has enough swapping space allocated to handle
a program whose pure code is 1.5 MB and whose data area is at
least 2.8 MB and can reach 100 MB or more.  If the swapping space is
insufficient, you will get an error in the command `temacs -batch -l
loadup dump', found in `./src/Makefile.in', or possibly when
running the final dumped Emacs.

Building Emacs requires about 140 MB of disk space (including the
Emacs sources) Once installed, Emacs occupies about 77 MB in the file
system where it is installed; this includes the executable files, Lisp
libraries, miscellaneous data files, and on-line documentation.  If
the building and installation take place in different directories,
then the installation procedure momentarily requires 140+77 MB.

2) Consult `./etc/MACHINES' to see what configuration name you should
give to the `configure' program.  That file offers hints for
getting around some possible installation problems.  The file lists
many different configurations, but only the part for your machine and
operating system is relevant.  (The list is arranged in alphabetical
order by the vendor name.)

3) You can build Emacs in the top-level Emacs source directory
or in a separate directory.

3a) To build in the top-level Emacs source directory, go to that
directory and run the program `configure' as follows:

    ./configure [CONFIGURATION-NAME] [--OPTION[=VALUE]] ...

The CONFIGURATION-NAME argument should be a configuration name given
in `./etc/MACHINES', with the system version number added at the end.

You should try first omitting CONFIGURATION-NAME.  This way,
`configure' will try to guess your system type.  If it cannot guess,
or if something goes wrong in building or installing Emacs this way,
try again specifying the proper CONFIGURATION-NAME explicitly.

If you don't want X support, specify `--with-x=no'.  If you omit this
option, `configure' will try to figure out for itself whether your
system has X, and arrange to use it if present.

The `--x-includes=DIR' and `--x-libraries=DIR' options tell the build
process where the compiler should look for the include files and
object libraries used with the X Window System.  Normally, `configure'
is able to find them; these options are necessary if you have your X
Window System files installed in unusual places.  These options also
accept a list of directories, separated with colons.

To get more attractive menus, you can specify an X toolkit when you
configure Emacs; use the option `--with-x-toolkit=TOOLKIT', where
TOOLKIT is `athena', `motif' or `gtk' (`yes' and `lucid' are synonyms
for `athena').  On some systems, it does not work to use a toolkit
with shared libraries.  A free implementation of Motif, called
LessTif, is available from <http://www.lesstif.org>.  Compiling with
LessTif or Motif causes a standard File Selection Dialog to pop up
when you invoke file commands with the mouse.  You can get fancy
3D-style scroll bars, even without LessTif/Motif, if you have the
Xaw3d library installed (see "Image support libraries" above for Xaw3d
availability).

If `--with-x-toolkit=gtk' is specified, you can tell configure where
to search for GTK by specifying `--with-pkg-config-prog=PATH' where
PATH is the pathname to pkg-config.  Note that GTK version 2.4 or
newer is required for Emacs.

The Emacs mail reader RMAIL is configured to be able to read mail from
a POP3 server by default.  Versions of the POP protocol older than
POP3 are not supported.  For Kerberos-authenticated POP add
`--with-kerberos', for Hesiod support add `--with-hesiod'.  While POP3
is always enabled, whether Emacs actually uses POP is controlled by
individual users--see the Rmail chapter of the Emacs manual.

For image support you may have to download, build, and install the
appropriate image support libraries for image types other than XBM and
PBM, see the list of URLs in "ADDITIONAL DISTRIBUTION FILES" above.
(Note that PNG support requires libz in addition to libpng.)

To disable individual types of image support in Emacs for some reason,
even though configure finds the libraries, you can configure with one
or more of these options:

  --without-xpm        for XPM image support
  --without-jpeg       for JPEG image support
  --without-tiff       for TIFF image support
  --without-gif        for GIF image support
  --without-png        for PNG image support

Use --without-toolkit-scroll-bars to disable LessTif/Motif or Xaw3d
scroll bars.

Use --without-xim to inhibit the default use of X Input Methods.  In
this case, the X resource useXIM can be used to turn on use of XIM.

Use --disable-largefile omits support for files larger than 2GB on
systems which support that.

Use --without-sound to disable sound support.

The `--prefix=PREFIXDIR' option specifies where the installation process
should put emacs and its data files.  This defaults to `/usr/local'.
- Emacs (and the other utilities users run) go in PREFIXDIR/bin
  (unless the `--exec-prefix' option says otherwise).
- The architecture-independent files go in PREFIXDIR/share/emacs/VERSION
  (where VERSION is the version number of Emacs, like `19.27').
- The architecture-dependent files go in
  PREFIXDIR/libexec/emacs/VERSION/CONFIGURATION
  (where CONFIGURATION is the configuration name, like mips-dec-ultrix4.2),
  unless the `--exec-prefix' option says otherwise.

The `--exec-prefix=EXECDIR' option allows you to specify a separate
portion of the directory tree for installing architecture-specific
files, like executables and utility programs.  If specified,
- Emacs (and the other utilities users run) go in EXECDIR/bin, and
- The architecture-dependent files go in
  EXECDIR/libexec/emacs/VERSION/CONFIGURATION.
EXECDIR/bin should be a directory that is normally in users' PATHs.

For example, the command

    ./configure mips-dec-ultrix --with-x11

configures Emacs to build for a DECstation running Ultrix, with
support for the X11 window system.

`configure' doesn't do any compilation or installation
itself.  It just creates the files that influence those things:
`./Makefile', `lib-src/Makefile', `oldXMenu/Makefile',
`lwlib/Makefile', `src/Makefile', and `./src/config.h'.  For details
on exactly what it does, see the section called `CONFIGURATION BY
HAND', below.

When it is done, `configure' prints a description of what it did and
creates a shell script `config.status' which, when run, recreates the
same configuration.  If `configure' exits with an error after
disturbing the status quo, it removes `config.status'.  `configure'
also creates a file `config.cache' that saves the results of its tests
to make reconfiguring faster, and a file `config.log' containing compiler
output (useful mainly for debugging `configure').  You can give
`configure' the option `--cache-file=FILE' to use the results of the
tests in FILE instead of `config.cache'.  Set FILE to `/dev/null' to
disable caching, for debugging `configure'.

If the description of the system configuration printed by `configure'
is not right, or if it claims some of the features or libraries are not
available when you know they are, look at the `config.log' file for
the trace of the failed tests performed by `configure' to check
whether these features are supported.  Typically, some test fails
because the compiler cannot find some function in the system
libraries, or some macro-processor definition in the system headers.

Some tests might fail because the compiler should look in special
directories for some header files, or link against optional
libraries, or use special compilation options.  You can force
`configure' and the build process which follows it to do that by
setting the variables CPPFLAGS, CFLAGS, LDFLAGS, LIBS, CPP and CC
before running `configure'.  CPP is the command which invokes the
preprocessor, CPPFLAGS lists the options passed to it, CFLAGS are
compilation options, LDFLAGS are options used when linking, LIBS are
libraries to link against, and CC is the command which invokes the
compiler.  By default, gcc is used if available.

Here's an example of a `configure' invocation, assuming a Bourne-like
shell such as Bash, which uses these variables:

 CPPFLAGS='-I/foo/myinclude' LDFLAGS='-L/bar/mylib' \
  CFLAGS='-O3' LIBS='-lfoo -lbar' ./configure

(this is all one long line).  This tells `configure' to instruct the
preprocessor to look in the `/foo/myinclude' directory for header
files (in addition to the standard directories), instruct the linker
to look in `/bar/mylib' for libraries, pass the -O3 optimization
switch to the compiler, and link against libfoo.a and libbar.a
libraries in addition to the standard ones.

For some libraries, like Gtk+, fontconfig and ALSA, `configure' use
pkg-config to find where those libraries are installed.
If you want pkg-config to look in special directories, you have to set
the environment variable PKG_CONFIG_PATH to point to the directories
where the .pc-files for those libraries are.
For example:

 PKG_CONFIG_PATH='/usr/local/alsa/lib/pkgconfig:/opt/gtk+-2.8/lib/pkgconfig' \
   ./configure

The work of `configure' can be done by editing various files in the
distribution, but using `configure' is easier.  See the section called
"CONFIGURATION BY HAND" below if you want to do the configuration
yourself.

3b) To build in a separate directory, go to that directory
and run the program `configure' as follows:

    SOURCE-DIR/configure CONFIGURATION-NAME [--OPTION[=VALUE]] ...

SOURCE-DIR refers to the top-level Emacs source directory which is
where Emacs's configure script is located.  `configure' looks for the
Emacs source code in the directory that `configure' is in.

To build in a separate directory, you must use a version of `make'
that supports the `VPATH' variable, such as GNU `make'.

3c) Some people try to build in a separate directory by filling
it full of symlinks to the files in the real source directory.
If you do that, `make all' does work, but `make install' fails:
it copies the symbolic links rather than the actual files.

As far as is known, there is no particular reason to use
a directory full of links rather than use the standard GNU
facilities to build in a separate directory (see 3b above).

4) Look at `./lisp/paths.el'; if some of those values are not right
for your system, set up the file `./lisp/site-init.el' with Emacs
Lisp code to override them; it is not a good idea to edit paths.el
itself.  YOU MUST USE THE LISP FUNCTION `setq' TO ASSIGN VALUES,
rather than `defvar', as used by `./lisp/paths.el'.  For example,

     (setq news-inews-program "/usr/bin/inews")

is how you would override the default value of the variable
news-inews-program (which is "/usr/local/inews").

Before you override a variable this way, *look at the value* that the
variable gets by default!  Make sure you know what kind of value the
variable should have.  If you don't pay attention to what you are
doing, you'll make a mistake.

5) Put into `./lisp/site-init.el' or `./lisp/site-load.el' any Emacs
Lisp code you want Emacs to load before it is dumped out.  Use
site-load.el for additional libraries if you arrange for their
documentation strings to be in the etc/DOC file (see
src/Makefile.in if you wish to figure out how to do that).  For all
else, use site-init.el.  Do not load byte-compiled code which
was build with a non-nil value of `byte-compile-dynamic'.

If you set load-path to a different value in site-init.el or
site-load.el, Emacs will use *precisely* that value when it starts up
again.  If you do this, you are on your own!

Note that, on some systems, the code you place in site-init.el must
not use expand-file-name or any other function which may look
something up in the system's password and user information database.
See `./etc/PROBLEMS' for more details on which systems this affects.

The `site-*.el' files are nonexistent in the distribution.  You do not
need to create them if you have nothing to put in them.

6) Refer to the file `./etc/TERMS' for information on fields you may
wish to add to various termcap entries.  The files `./etc/termcap.ucb'
and `./etc/termcap.dat' may already contain appropriately-modified
entries.

7) Run `make' in the top directory of the Emacs distribution to finish
building Emacs in the standard way.  The final executable file is
named `src/emacs'.  You can execute this file "in place" without
copying it, if you wish; then it automatically uses the sibling
directories ../lisp, ../lib-src, ../info.

Or you can "install" the executable and the other Emacs into their
installed locations, with `make install'.  By default, Emacs's files
are installed in the following directories:

`/usr/local/bin' holds the executable programs users normally run -
		`emacs', `etags', `ctags', `b2m', `emacsclient',
		and `rcs-checkin'.

`/usr/local/share/emacs/VERSION/lisp' holds the Emacs Lisp library;
		`VERSION' stands for the number of the Emacs version
		you are installing, like `18.59' or `19.27'.  Since the
		Lisp library changes from one version of Emacs to
		another, including the version number in the path
		allows you to have several versions of Emacs installed
		at the same time; in particular, you don't have to
		make Emacs unavailable while installing a new version.

`/usr/local/share/emacs/VERSION/etc' holds the Emacs tutorial, the DOC
		file, the `yow' database, and other
		architecture-independent files Emacs might need while
		running.  VERSION is as specified for `.../lisp'.

`/usr/local/libexec/emacs/VERSION/CONFIGURATION-NAME' contains executable
		programs used by Emacs that users are not expected to
		run themselves.
		`VERSION' is the number of the Emacs version you are
		installing, and `CONFIGURATION-NAME' is the argument
		you gave to the `configure' program to identify the
		architecture and operating system of your machine,
		like `mips-dec-ultrix' or `sparc-sun-sunos'.  Since
		these files are specific to the version of Emacs,
		operating system, and architecture in use, including
		the configuration name in the path allows you to have
		several versions of Emacs for any mix of machines and
		operating systems installed at the same time; this is
		useful for sites at which different kinds of machines
		share the file system Emacs is installed on.

`/usr/local/share/info' holds the on-line documentation for Emacs,
		known as "info files".  Many other GNU programs are
		documented using info files as well, so this directory
		stands apart from the other, Emacs-specific
		directories.

`/usr/local/man/man1' holds the man pages for the programs installed
		in `/usr/local/bin'.

Any version of Emacs, whether installed or not, also looks for Lisp
files in these directories.

`/usr/local/share/emacs/VERSION/site-lisp' holds the local Emacs Lisp
		files installed for Emacs version VERSION only.

`/usr/local/share/emacs/site-lisp' holds the local Emacs Lisp
		files installed for all Emacs versions.

		When Emacs is installed, it searches for its Lisp files
		in `/usr/local/share/emacs/VERSION/site-lisp', then in
		`/usr/local/share/emacs/site-lisp', and finally in
		`/usr/local/share/emacs/VERSION/lisp'.

If these directories are not what you want, you can specify where to
install Emacs's libraries and data files or where Emacs should search
for its Lisp files by giving values for `make' variables as part of
the command.  See the section below called `MAKE VARIABLES' for more
information on this.

8) Check the file `dir' in your site's info directory (usually
/usr/local/share/info) to make sure that it has a menu entry for the
Emacs info files.

9) If your system uses lock files to interlock access to mailer inbox files,
then you might need to make the movemail program setuid or setgid
to enable it to write the lock files.  We believe this is safe.

10) You are done!  You can remove executables and object files from
the build directory by typing `make clean'.  To also remove the files
that `configure' created (so you can compile Emacs for a different
configuration), type `make distclean'.  If you don't need some, or all
of the input methods from the Leim package, you can remove the
unneeded files in the leim subdirectories of your site's lisp
directory (usually /usr/local/share/emacs/VERSION/).



MAKE VARIABLES

You can change where the build process installs Emacs and its data
files by specifying values for `make' variables as part of the `make'
command line.  For example, if you type

    make install bindir=/usr/local/gnubin

the `bindir=/usr/local/gnubin' argument indicates that the Emacs
executable files should go in `/usr/local/gnubin', not
`/usr/local/bin'.

Here is a complete list of the variables you may want to set.

`bindir' indicates where to put executable programs that users can
	run.  This defaults to /usr/local/bin.

`datadir' indicates where to put the architecture-independent
	read-only data files that Emacs refers to while it runs; it
	defaults to /usr/local/share.  We create the following
	subdirectories under `datadir':
	- `emacs/VERSION/lisp', containing the Emacs Lisp library, and
	- `emacs/VERSION/etc', containing the Emacs tutorial, the DOC
		file, and the `yow' database.
	`VERSION' is the number of the Emacs version you are installing,
	like `18.59' or `19.0'.  Since these files vary from one version
	of Emacs to another, including the version number in the path
	allows you to have several versions of Emacs installed at the
	same time; this means that you don't have to make Emacs
	unavailable while installing a new version.

`libexecdir' indicates where to put architecture-specific data files that
	Emacs refers to as it runs; it defaults to `/usr/local/libexec'.
	We create the following subdirectories under `libexecdir':
	- `emacs/VERSION/CONFIGURATION-NAME', containing executable
		programs used by Emacs that users are not expected to run
		themselves.
	`VERSION' is the number of the Emacs version you are installing,
	and `CONFIGURATION-NAME' is the argument you gave to the
	`configure' program to identify the architecture and operating
	system of your machine, like `mips-dec-ultrix' or
	`sparc-sun-sunos'.  Since these files are specific to the version
	of Emacs, operating system, and architecture in use, including
	the configuration name in the path allows you to have several
	versions of Emacs for any mix of machines and operating systems
	installed at the same time; this is useful for sites at which
	different kinds of machines share the file system Emacs is
	installed on.

`infodir' indicates where to put the info files distributed with
	Emacs; it defaults to `/usr/local/share/info'.

`mandir' indicates where to put the man pages for Emacs and its
	utilities (like `etags'); it defaults to
	`/usr/local/man/man1'.

`manext' gives the extension the man pages should be installed with.
	It should contain a period, followed by the appropriate
	digit.  It defaults to `.1'.  For example given the default
	values for `mandir' and `manext', the Emacs man page would be
	installed as `/usr/local/man/man1/emacs.1'.

`prefix' doesn't give a path for any specific part of Emacs; instead,
	its value is used to determine the defaults for all the
	architecture-independent path variables - `datadir',
	`sharedstatedir', `infodir', and `mandir'.  Its default value is
	`/usr/local'; the other variables add on `lib' or `man' to it
	by default.

	For example, suppose your site generally places GNU software
	under `/usr/users/software/gnusoft' instead of `/usr/local'.
	By including
	    `prefix=/usr/users/software/gnusoft'
	in the arguments to `make', you can instruct the build process
	to place all of the Emacs data files in the appropriate
	directories under that path.

`exec_prefix' serves the same purpose as `prefix', but instead
	determines the default values for the architecture-dependent
	path variables - `bindir' and `libexecdir'.

The above variables serve analogous purposes in the makefiles for all
GNU software; this variable is specific to Emacs.

`archlibdir' indicates where Emacs installs and expects the executable
	files and other architecture-dependent data it uses while
	running.  Its default value, based on `libexecdir' (which
	see), is `/usr/local/libexec/emacs/VERSION/CONFIGURATION-NAME'
	(where VERSION and CONFIGURATION-NAME are as described above).

Remember that you must specify any variable values you need each time
you run `make' in the top directory.  If you run `make' once to build
emacs, test it, and then run `make' again to install the files, you
must provide the same variable settings each time.  To make the
settings persist, you can edit them into the `Makefile' in the top
directory, but be aware that running the `configure' program erases
`Makefile' and rebuilds it from `Makefile.in'.

The path for finding Lisp files is specified in src/paths.h,
a file which is generated by running configure.  To change the path,
you can edit the definition of PATH_LOADSEARCH in that file
before you run `make'.

The top-level Makefile stores the variable settings it used in the
Makefiles for the subdirectories, so you don't have to specify them
when running make in the subdirectories.


CONFIGURATION BY HAND

Instead of running the `configure' program, you have to perform the
following steps.

1) Copy `./src/config.in' to `./src/config.h'.

2) Consult `./etc/MACHINES' to see what configuration name you should
use for your system.  Look at the code of the `configure' script to
see which operating system and architecture description files from
`src/s' and `src/m' should be used for that configuration name.  Edit
`src/config.h', and change the two `#include' directives to include
the appropriate system and architecture description files.

2) Edit `./src/config.h' to set the right options for your system.  If
you need to override any of the definitions in the s/*.h and m/*.h
files for your system and machine, do so by editing config.h, not by
changing the s/*.h and m/*.h files.  Occasionally you may need to
redefine parameters used in `./lib-src/movemail.c'.

3) Create src/Makefile and lib-src/Makefile from the corresponding
`Makefile.in' files.  First copy `Makefile.in' to `Makefile.c',
then edit in appropriate substitutions for the @...@ constructs,
and then copy the shell commands near the end of `configure'
that run cpp to construct `Makefile'.

4) Create `Makefile' files in various other directories
from the corresponding `Makefile.in' files.  This isn't so hard,
just a matter of substitution.

The `configure' script is built from `configure.in' by the `autoconf'
program.  You need version 2.51 or newer of `autoconf' to rebuild
`configure'.

BUILDING GNU EMACS BY HAND

Once Emacs is configured, running `make' in the top directory performs
the following steps.

1) Run `make src/paths.h' in the top directory.  This produces
`./src/paths.h' from the template file `./src/paths.in', changing
the paths to the values specified in `./Makefile'.

2) Go to directory `./lib-src' and run `make'.  This creates
executables named `ctags' and `etags' and `make-docfile' and
`digest-doc' and `test-distrib'.  And others.

3) Go to directory `./src' and Run `make'.  This refers to files in
the `./lisp' and `./lib-src' subdirectories using names `../lisp' and
`../lib-src'.

This creates a file `./src/emacs' which is the runnable Emacs,
which has another name that contains a version number.
Each time you do this, that version number increments in the last place.

It also creates a file in `./etc' whose name is `DOC' followed by the
current Emacs version.  This file contains documentation strings for
all the functions in Emacs.  Each time you run make to make a new
emacs, a new DOC file with a new name is made.  You must keep the DOC
file for an Emacs version as long as you keep using that Emacs
version.


INSTALLATION BY HAND

The steps below are done by running `make install' in the main
directory of the Emacs distribution.

1) Copy `./lisp' and its subdirectories, `./etc', and the executables
in `./lib-src' to their final destinations, as selected in `./src/paths.h'.

Strictly speaking, not all of the executables in `./lib-src' need be copied.
- The programs `fakemail', `hexl', `movemail', `profile', `rcs2log',
    and `vcdiff' are used by Emacs; they do need to be copied.
- The programs `etags', `ctags', `emacsclient', `b2m', and `rcs-checkin'
    are intended to be run by users; they are handled below.
- The programs `make-docfile' and `test-distrib' were
    used in building Emacs, and are not needed any more.
- The programs `digest-doc' and `sorted-doc' convert a `DOC' file into
    a file for users to read.  There is no important reason to move them.

2) Copy the files in `./info' to the place specified in
`./lisp/site-init.el' or `./lisp/paths.el'.  Note that if the
destination directory already contains a file named `dir', you
probably don't want to replace it with the `dir' file in the Emacs
distribution.  Instead, you should make sure that the existing `dir'
file contains an appropriate menu entry for the Emacs info.

3) Copy `./src/emacs' to `/usr/local/bin', or to some other directory
in users' search paths.  `./src/emacs' has an alternate name
`./src/emacs-EMACSVERSION'; you may wish to make a symbolic link named
`/usr/local/bin/emacs' pointing to that alternate name, as an easy way
of installing different versions.

You can delete `./src/temacs'.

4) Copy the programs `b2m', `emacsclient', `ctags', `etags', and
`rcs-checkin' from `./lib-src' to `/usr/local/bin'.  These programs are
intended for users to run.

5) Copy the man pages in `./etc' for emacs, ctags, and etags into the
appropriate man directories.

6) The files in the `./src' subdirectory, except for `emacs', are not
used by Emacs once it is built.  However, it is very desirable to keep
the source on line for debugging.


PROBLEMS

See the file PROBLEMS in etc subdirectory for a list of various
problems sometimes encountered, and what to do about them.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
