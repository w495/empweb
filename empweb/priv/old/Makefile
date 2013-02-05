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


	find -type f -name \* -exec sed -i -r s/cowboy_http_websocket_empweb_handler/cowboy_http_websocket_handler/g {} \;
empweb_mailutils

	find -type f -name \* -exec sed -i -r s/empweb_api/empweb_jsonapi/g {} \;


	find -type f -name \* -exec sed -i -r s/lpgs/lgps/g {} \;



	find -type f -name \* -exec sed -i -r s/lpgs/empfuntest/g {} \;

	
	find -type f -name \* -exec sed -i -r s/empdb_lgps/lgps/g {} \;

	find -type f -name \* -exec sed -i -r s/empweb_jsonapi:norm/empweb_norm:norm/g {} \;

	
BIZ_SESSION_EXPIRE_TIMEOUT

	find -type f -name \* -exec sed -i -r s/demo/udptv/g {} \;

	find -type f -name \* -exec sed -i -r s/Demo/Udptv/g {} \;

	find -type f -name \* -exec sed -i -r s/empdb_empweb_biz_/empdb_biz_/g {} \;

	
	find . -type f -name "*.erl" | awk 'FS=\.{print $1}' | xargs -t -i mv {} {}.old

	find -type f -name \* -exec sed -i -r s/empdb_biz/empdb_biz/g {} \;


	find ./ -type f -exec sh -c 'mv {} `echo {} | sed "s/.mp3/_old.mp3/"` ' \ ;


	$ find ./ -type f -exec sh -c 'mv {} `echo {} | xargs basename | sed "s/^/empweb_/"`' \ ;



	ls | sed s/empweb_//g |  xargs -i echo ./empweb_empweb_{} ./empweb_{}



	ls | sed s/demo_//g |  xargs -i echo ./demo_{} ./udptv_{}


	ls | sed s/diplom//g |  xargs -i echo ./diplom{} ./udptv_{}


	
	find ./ -type f -exec sh -c 'mv {} `echo {} | sed "s/diplom/scref/"` ' \ ;




	ls | sed s/empweb_//g |  xargs -i mv ./empweb{} ./taweb_{}

	find -type f -name \* -exec sed -i -r s/empweb/taweb/g {} \;


	find ./ -type f -exec sh -c 'mv {} `echo {} | sed "s/.mp3/_old.mp3/"` ' \ ;


	find -type f -name \* -exec sed -i -r s/EMPWEB/TAWEB/g {} \;