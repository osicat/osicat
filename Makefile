# Boilerplate project makefile for Common-lisp.net
# May all your snapshots be orderly. ;)
#
# Per-project files needed: version.txt, release.txt
# Per-developer files needed: username.txt
#
# Following directory structure is assumed, relative to
# the directory of this Makefile (assumed to be in your
# source directory).
#
# ../public_html/
# ../ftp/
#

# Fill in you project name
PROJECT=osicat

# Release version number
VERSION=`cat version.txt`
# Username
USERNAME=`cat username.txt`
# List of files included in release
FILES=`cat release.txt`

HTML=../public_html
FTP=../ftp
DATE=`date +%F`
NAME=$(PROJECT)_$(VERSION)
LINK=$(PROJECT)_latest.tar.gz
SNAPSHOT=$(PROJECT)_$(DATE)
SNAPLINK=$(PROJECT)_latest-snapshot.tar.gz

CLNET=$(USERNAME)@common-lisp.net
CLNET_HOME=/project/$(PROJECT)
CLNET_FTP=$(CLNET_HOME)/ftp/
CLNET_HTML=$(CLNET_HOME)/public_html/

FTP_PERMS=ssh $(CLNET) "chgrp -R $(PROJECT) $(CLNET_FTP) && chmod -R ug+rw,o-w $(CLNET_FTP)/*"
HTML_PERMS=ssh $(CLNET) "chgrp -R $(PROJECT) $(CLNET_HTML) && chmod -R ug+rw,o-w $(CLNET_HTML)*"
RSYNC=rsync -vlcrC 
RSYNC_FTP=$(RSYNC) $(FTP)/. $(CLNET):$(CLNET_FTP)
RSYNC_HTML=$(RSYNC) $(HTML)/. $(CLNET):$(CLNET_HTML)

.PHONY: snapshot release public_html

all:
	@echo available targets: 
	@echo
	@echo "   public_html"
	@echo
	@echo "   snapshot: $(SNAPSHOT)"
	@echo "   release:  $(NAME)"
	@echo	

snapshot:
	mkdir -p $(SNAPSHOT)
	cp $(FILES) $(SNAPSHOT)/
	tar -czvf $(SNAPSHOT).tar.gz $(SNAPSHOT)
	gpg -b -a $(SNAPSHOT).tar.gz
	rm -rf $(SNAPSHOT)
	ln -s $(SNAPSHOT).tar.gz.asc $(SNAPLINK).asc
	ln -s $(SNAPSHOT).tar.gz $(SNAPLINK)
	mv $(SNAPSHOT).tar.gz $(SNAPSHOT).tar.gz.asc $(SNAPLINK) $(SNAPLINK).asc $(FTP)
#	$(RSYNC_FTP) && $(FTP_PERMS)

release:
	mkdir -p $(NAME)
	cp $(FILES) $(NAME)/
	tar -czvf $(NAME).tar.gz $(NAME)
	gpg -b -a $(NAME).tar.gz
	rm -rf $(NAME)
	ln -s $(NAME).tar.gz.asc $(LINK).asc
	ln -s $(NAME).tar.gz $(LINK)
	mv $(NAME).tar.gz $(NAME).tar.gz.asc $(LINK) $(LINK).asc $(HTML)/files/
#	$(RSYNC_FTP) && $(FTP_PERMS)

public_html:
#	$(RSYNC_HTML) && $(HTML_PERMS)
