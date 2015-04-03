CC=gcc
# gcc-wflags=-Wall -Wno-unused -Wshadow -Wwrite-strings \
#  -Wnested-externs -Winline -Wpointer-arith -Wcast-qual \
#   -pedantic -Waggregate-return -Wstrict-prototypes -Wconversion 

#-Wredundant-decls -Wcast-align -Wbad-function-cast -W 
pflags=$(gcc-wflags)
# We expect pflags to be defined in ENV if necessary.  It provides a
# place to put platform-specific switches, such as (see eclipse.h):
#    export pflags="-malign-double -DALIGN_DOUBLE"
# On some platforms, there may not be enough space in /tmp, so it may
# also be necessary to also export TMPDIR=`pwd` 
#CFLAGS=-I. -O2 $(pflags)
CFLAGS=-I. -g $(pflags)
ADDITIONAL_LINTFLAGS=-i
LINTFLAGS=-DLINT $(LINT_FLAGS)

# N.B.: To compile the Eclipse source with -traditional on Linux, you
# may need to edit out the last definition (nonosleep) in <time.h> (It
# is ok to edit a local copy (i.e. "time.h") -- the reference to
# <time.h> will still pick up the local version.)  

#########################################################################
# The distribution has a c directory that contains header files, the
# example eclipse.c (containing main()), and in some cases, the C
# source code for all or part of eclipse.  This directory also
# contains the master makefile.
#
# The distribution also has a bin directory that has subdirectories
# for each processor family.  These subdirectories contain all the
# binaries such as the libraries and the eclipse executable.  They may
# also contain individiual .o files.  The sources in these
# directories, if available, are links to the sources in the c
# directory.
# 
# N.B.: Except as noted, each target is intended to be used ONLY
# within the appropriate bin subdirectory, not within the c directory.
#########################################################################

#########################################################################
# INSTALLATION LOCATIONS
# where 'make setup' should put include, library, and executable
# files.
# N.B.: None of this is needed to recompile eclipse -- it is used only
# for installing the public results into a public place.
#########################################################################
INSTALLDIR=/usr/local
# N.B.: gcc will search /usr/local/include, which may be a directory with
# more open writing permissions than /usr/include.  However, other cc
# will usually search only /usr/include.
INCLUDEDIR=$(INSTALLDIR)/include
LIBDIR=$(INSTALLDIR)/lib
BINDIR=$(INSTALLDIR)/bin

#########################################################################
# MISSING DEFINITIONS
# These are not defined on some platforms:

# Missing on HP
COMPILE.c = $(CC) $(CFLAGS) $(CPPFLAGS) -c
AR = ar
ARFLAGS = rv
RM = rm -f

# Missing on Motorola
#LINT=lint
#LINT.c=$(LINT) $(LINTFLAGS) $(CPPFLAGS) $(TARGET_ARCH)
#.SUFFIXES: .ln

#########################################################################
# THE SOURCE FILES AND THEIR DEPENDENCIES
#########################################################################

sources = interface.c common.c parameters.c mop-init.c kernel.c		\
types-run.c control-run.c symbol.c classes.c clos-run.c clos-define.c	\
list.c clos-seq.c mop.c class-meth.c methods.c gfunc.c method-init.c	\
predicates.c arithmetic.c conv.c hash.c type-ops.c type-seq.c		\
method-comb.c type-mops.c sequence.c seq-mod.c search.c sort.c		\
control.c numbers.c trig.c num-conv.c array.c string.c struct-run.c	\
character.c tree.c resource.c condition.c set.c alist.c bit-array.c	\
bignum.c bits.c equalp.c package.c stream.c file-stream.c		\
comp-stream.c circle.c init.c reader.c dispatch.c printer.c pretty.c	\
format.c print-object.c doc.c describer.c miscel.c random.c pathname.c	\
file.c enclose.c pkg.c \
c-pkg.c env.c

dsources = dev-meth.c env-comp.c common-comp.c prog-comp.c	\
cont-comp.c cont-comp2.c macro.c macro-comp.c opts.c number-comp.c	\
list-comp.c clos-comp.c pretty-comp.c struct-comp.c cond-comp.c		\
more-compile.c loop.c directives.c describe.c debug.c default-pp.c	\
walk-util.c walk-top.c walk-special.c literal.c evaluation.c c.c	\
file-walk.c c-walk.c prim-decs.c

static-sources = static-class.c statics-ext.c statics-int.c statics-add.c

objects =        $(sources:.c=.o)
dobjects =       $(dsources:.c=.o)
static-objects = $(static-sources:.c=.o)
lints =          $(sources:.c=.ln) $(dsources:.c=.ln) eclipse.ln initd.ln
static-lints =   $(static-sources:.c=.ln)

l = libeclipse

# The default target must be first
test: eclipse
#	./eclipse -n

$(objects) $(dobjects) $(static-objects) $(lints) eclipse.o initd.o: eclipse.h 

interface.o $(static-objects) $(static_lints): statics.h

#interface.o interface.ln: gc.h

character.o character.ln: chars.h

#########################################################################
# TARGETS
#########################################################################
# The LIBDL hair is to handle broken dl library name on HP.
eclipse: $(l).a $(l)d.a  eclipse.o 
	if [ `uname -s` = "HP-UX" ]; then LIBDL=dld; else LIBDL=dl; fi; \
	$(CC) $(CFLAGS) $(LDFLAGS) -L. eclipse.o -o eclipse -leclipse -leclipsed -lm -l$$LIBDL -lgc

# See bottom of file for alternatives that don't keep .o files lying around.
$(l).a: $(objects) $(static-objects) 
	$(AR) $(ARFLAGS)l $@ $?
	ranlib $@

$(l)d.a: $(dobjects) initd.o
	$(AR) $(ARFLAGS)l $@ $?
	ranlib $@

libgc.a:
	xxx=`pwd`; echo xxx=$$xxx; cd ../../c; \
	echo Linking gc files from `pwd` to $$xxx; \
	find gc -depth -print | cpio -pdl $$xxx
	cd gc; echo Making gc test in `pwd`; make test
	@ echo $@ is now incorporated into $(l).a > $@
	mv gc/gc.a $(l).a

unarchive:
	$(AR) xv $(l).a
	$(AR) xv $(l)d.a

$(sources) $(dsources) $(static-sources) eclipse.h statics.h chars.h eclipse.c chars.c initd.c:
	ln -s ../../c/$@ .	

gc.h:	
	ln -fs ../../c/gc/gc.h $@

# Recompilation
recompile: clean clean-auxiliary clean-source
	make test
	make setup 

# Performs a rebuild with clObject defined as a union instead of pointers.
# N.B.: not currently working!
careful: 
	make CFLAGS="-I. -O2 -Dcl_CTYPECHECKS -fpcc-struct-return $(gcc-wflags)"

# Link files to standard places
setup: $(INCLUDEDIR)/eclipse.h $(INCLUDEDIR)/gc.h	\
 $(LIBDIR)/$(l).a $(LIBDIR)/$(l)d.a 			\
 $(BINDIR)/eclipse
	@ echo Binary and header files linked.

$(INCLUDEDIR)/eclipse.h: eclipse.h
	make $(INCLUDEDIR)
	ln -s `pwd`/$? $@

$(INCLUDEDIR)/gc.h: gc.h
	make $(INCLUDEDIR)
	ln -s `pwd`/$? $@

$(LIBDIR)/$(l).a: $(l).a
	make $(LIBDIR)
	ln -s `pwd`/$? $@

$(LIBDIR)/$(l)d.a: $(l)d.a
	make $(LIBDIR)
	ln -s `pwd`/$? $@

$(BINDIR)/eclipse: eclipse
	make $(BINDIR)
	ln -s `pwd`/$? $@

$(INCLUDEDIR) $(LIBDIR) $(BINDIR):
	mkdir $@

# These are intended to be used only from the c directory.  
# They make the apporiate bin subdirectory if necessary, linking the
# makefile, and then switch to that directory to do a make.
sparc hppa linux: 
	make ../bin/$@
	@ cd ../bin/$@; echo Making eclipse in `pwd`; make eclipse

../bin/sparc ../bin/hppa ../bin/linux: ../bin
	mkdir $@
	ln -s ../../c/makefile $@

../bin:
	mkdir $@

install:
	mach=`uname -s`;					\
	if [ $$mach = "HP-UX" ]; then ECLIPSEBIN=hppa; 		\
	else if [ $$mach = "Linux" ]; then ECLIPSEBIN=linux;	\
	else ECLIPSEBIN=sparc; fi; fi;				\
	echo Making $$ECLIPSEBIN; make $$ECLIPSEBIN;		\
	cd ../bin/$$ECLIPSEBIN; make setup dist-clean1
	@ echo Installation complete.


#########################################################################
# PROGRAMMING UTILITIES
#########################################################################
lint: $(lints) eclipse.h gc.h chars.h makefile
	lint $(LINT_FLAGS) -u $(lints) 

TAGS: $(sources) $(dsources) $(static-sources) eclipse.c eclipse.h statics.h chars.h initd.c
	etags $(sources) $(dsources) $(static-sources) eclipse.c eclipse.h statics.h chars.h initd.c

#########################################################################
# HOUSE CLEANING
#########################################################################
clean-objects: 
	$(RM) $(objects) $(static-objects) eclipse.o 
	$(RM) eclipse
	$(RM) $(l).a $(l)d.a 

clean-lint: 
	$(RM) $(lints)

clean:	clean-objects clean-lint
	$(RM) gc.h libgc.a 
	cd gc; make clean

# get rid of sources so we can get them again from the master
# directory
clean-source:
	$(RM) *.[ch]
	$(RM) -r gc


# Destroy symbolic links or copies in standard places.
clean-auxiliary:
	$(RM) $(INCLUDEDIR)/eclipse.h $(INCLUDEDIR)/gc.h 
	$(RM) $(LIBDIR)/$(l).a $(LIBDIR)/$(l)d.a 
	$(RM) $(BINDIR)/eclipse


# Remove links that go back to the source directory, see below.
# Also remove object files and gc directory, all of which should be in
# the archive(s).
dist-clean1:
	$(RM) *.c statics.h chars.h
	$(RM) *.o
	$(RM) -r gc

# Only used from c directory at Elwood to remove waste before making the CD
dist-clean:
	cd ../bin/sparc; make dist-clean1
	cd ../bin/hppa; make dist-clean1
	cd ../bin/linux; make dist-clean1
	make archive-c
	rm -rf *~

# Delete non-eclipse host lisp binaries in parent directory.
# Used for Eclipse development
clean-bin:
	cd ../../../.. ; $(RM) *.sparcf *.sparc *.hpf *.fasl \
	$(RM) test/*.sparcf test/*.sparc test/*.hpf test/*.fasl

open-sources = interface.c initd.c eclipse.c statics-ext.c statics-int.c statics-add.c chars.c

save-dir:
	mkdir $@

save-c: $(open-sources) 
	make save-dir
	mv $? save-dir

restore-c: save-dir/*
	mv save-dir/* .
	rmdir save-dir

# The next two delete all generated C source code!!!
clean-c: 
	make save-c
	$(RM) *.c
	make restore-c

archive-c:
	make save-c
	tar --create --verbose --gzip --file=c-dsource.tgz $(dsources)
	crypt < c-dsource.tgz > c-dsource.encrypted
	rm $(dsources) c-dsource.tgz
	tar --create --verbose --gzip --file=c-source.tgz *.c
	crypt < c-source.tgz > c-source.encrypted
	rm *.c c-source.tgz
	make restore-c

unarchive-runtime:
	crypt < c-source.encrypted > c-source.tgz
	tar --extract --verbose --gunzip --file=c-source.tgz
	rm c-source.tgz c-source.encrypted

unarchive-development:
	crypt < c-dsource.encrypted > c-dsource.tgz
	tar --extract --verbose --gunzip --file=c-dsource.tgz
	rm c-dsource.tgz c-dsource.encrypted

unarchive-c:
	make unarchive-development
	make unarchive-runtime

clean-all: clean clean-c clean-bin

#########################################################################
# ADDITIONAL RULES
#########################################################################
# This is not defined on some platforms (Motorola).
.c.ln: eclipse.h
	$(LINT.c) $(OUTPUT_OPTION) $(ADDITIONAL_LINTFLAGS) $<

#########################################################################
# ALTERNATIVE APPROACHES
#########################################################################
# ll = $(l).a
# IWBNI we didn't keep .o files around and used .c.a/lib(x.o) rules to
# update the libraries directly.  Alas, there are a number of problems
# with this approach:
#
# 1. Errors during ar commands can sometimes result in the total loss
#    of the archive.  This is too expensive to rebuild if you don't
#    have the .o files around.  (The ar command should operate on a
#    copy in case it is aborted.  We could try defining AR to be a
#    simple script that does this, but then there's still all the
#    other issues.)
#
# 2. Some platforms (HP) don't support lib(x.o y.o ... z.o) syntax,
#    requiring us to to write rules like $(ll): $(ll)(x.o) $(ll)(y.o) ... $(ll)(z.o)
#    instead of simply $(ll): $(ll)($(objects) $(static-objects)).  This
#    duplication is unacceptable.  
#
#    Technically, we would have to also duplicate the targets of the
#    form $(ll)($(xxx)): yyy.h to repeat all the left-hand-side archive
#    members, too, though $(ll)($(xxx)) seems to work anyway, PROVIDED we
#    supply a trailing space after the last $(xxx) reference and before
#    the closing paren.
# 
# 3. The built in .c.a rules are unreliable, with some platforms
#    apparently failing to clean up the .o files, which, after all, is
#    the whole point.
# 
# 4. The built in .c.a rules are slow, with each archive member being
#    compiled and ar rv'd individually. It is much faster to supress
#    the default .c.a rule, and instead supply a $(ll): rule that
#    compiles and archives all the out of date files in one shot.
#    Alas, to be able to do this requires that $? be defined to be the
#    out of date archive members in a rule like $(ll): $(ll)($(objects)
#    $(static-objects)).  Some platforms (linux), fail to define $?
#    properly in this situation.

# To try any of this stuff:
# 1. replace the $(ll): rule, above, with what follows.
# 2. replace the xxx.o references that set up dependencies on eclipse.h
#    statics.h and gc.h, with $(ll)(xxx.o ).  (Note that most of
#    the xxx.o references don't appear directly as targets, but are
#    instead of the form $(objects) and $(static-objects). Make sure
#    to include the trailing space before the last paren in $(ll)(... )

# If it were not for broken make implemantions (HP) that do not allow
# lib(x.o y.o) syntax, the repetition, below, could be replaced by:
#$(ll): $(ll)($(objects) $(static-objects) )

#$(ll): $(ll)(interface.o) $(ll)(common.o) $(ll)(kernel.o) $(ll)(types-run.o)		\
#$(ll)(control-run.o) $(ll)(symbol.o) $(ll)(list.o) $(ll)(classes.o) $(ll)(clos-run.o)	\
#$(ll)(clos-define.o) $(ll)(clos-seq.o) $(ll)(methods.o) $(ll)(predicates.o)		\
#$(ll)(arithmetic.o) $(ll)(hash.o) $(ll)(type-ops.o) $(ll)(type-seq.o)		\
#$(ll)(method-comb.o) $(ll)(type-mops.o) $(ll)(control.o) $(ll)(sequence.o)		\
#$(ll)(numbers.o) $(ll)(array.o) $(ll)(string.o) $(ll)(struct-run.o)			\
#$(ll)(character.o) $(ll)(struct-comp.o) $(ll)(resource.o) $(ll)(tree.o)		\
#$(ll)(cond-comp.o) $(ll)(env.o) $(ll)(package.o) $(ll)(c-pkg.o) $(ll)(condition.o)	\
#$(ll)(set.o) $(ll)(alist.o) $(ll)(bit-array.o) $(ll)(bignum.o) $(ll)(bits.o)		\
#$(ll)(more-compile.o) $(ll)(equalp.o) $(ll)(stream.o) $(ll)(circle.o) $(ll)(loop.o)	\
#$(ll)(init.o) $(ll)(reader.o) $(ll)(printer.o) $(ll)(format.o) $(ll)(pretty.o)		\
#$(ll)(print-object.o) $(ll)(describe.o) $(ll)(doc.o) $(ll)(debug.o) $(ll)(miscel.o)	\
#$(ll)(random.o) $(ll)(pathname.o) $(ll)(file.o) $(ll)(dispatch.o) $(ll)(default-pp.o)	\
#$(ll)(parameters.o) $(ll)(prog-comp.o) $(ll)(cont-comp.o) $(ll)(cont-comp2.o)	\
#$(ll)(number-comp.o) $(ll)(list-comp.o) $(ll)(clos-comp.o) $(ll)(pretty-comp.o)	\
#$(ll)(macro-comp.o) $(ll)(walk.o) $(ll)(literal.o) $(ll)(c.o) $(ll)(file-walk.o)	\
#$(ll)(c-walk.o) $(ll)(evaluation.o) $(ll)(common-comp.o) $(ll)(macro.o) $(ll)(pkg.o)	\
#$(ll)(prim-decs.o) $(ll)(static-class.o) $(ll)(statics-ext.o) $(ll)(statics-int.o)	\
#$(ll)(statics-add.o)
#	ranlib $

# If linux supported $? properly for l(xx.o) dependencies, IWBNI
# (faster) to insert: 
#	$(COMPILE.c) -o $? $(?:.o=.c)
#	$(AR) $(ARFLAGS) $@ $?
#	$(RM) $?
# before the ranlib command, above, and then override the built in
# rule for .c.a by specifying:
#.c.a:;
