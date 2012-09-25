# default

RIAC=./generate.py 
RIAHOME=.

OLDAPPNAME=zqa
NEWAPPNAME=zqr

SOURCEPATH=$(RIAHOME)/source
CACHEPATH=$(RIAHOME)/cache
BUILDPATH=$(RIAHOME)/build

CLASSPATH=$(SOURCEPATH)/class
RESOURCEPATH=$(SOURCEPATH)/resource

ifdef o
OLDAPPNAME=$(o)
endif

ifdef n
OLDAPPNAME=$(n)
endif

RM=rm
RMFLAGS=-rfv

all:
	(cd $(RIAHOME)/ && $(RIAC) build) || exit 1

debug:
	(cd $(RIAHOME)/ && $(RIAC) source) || exit 1

clean: cleanria

cleanria:
	$(RM) $(RMFLAGS) $(CACHEPATH)/
	$(RM) $(RMFLAGS) $(BUILDPATH)/

rename:
	# $(OLDAPPNAME) -> $(NEWAPPNAME)
	mv $(CLASSPATH)/$(OLDAPPNAME) $(CLASSPATH)/$(NEWAPPNAME);
	mv $(RESOURCEPATH)/$(OLDAPPNAME) $(RESOURCEPATH)/$(NEWAPPNAME);
	cd $(SOURCEPATH)/ && find -type f -name \* -exec sed -i -r s/$(OLDAPPNAME)/$(NEWAPPNAME)/g {} \;
