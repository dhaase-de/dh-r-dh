PACKAGE_NAME=dh
PACKAGE_VERSION=$(shell grep -o "^Version:.*$$" $(PACKAGE_NAME)/DESCRIPTION | sed 's/Version:\s\+\(.*\)$$/\1/g')

install: clean check
	R CMD INSTALL $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz

check:
	R CMD check $(PACKAGE_NAME)
	R CMD build $(PACKAGE_NAME)

clean:
	rm -f $(PACKAGE_NAME)_$(PACKAGE_VERSION).tar.gz
	rm -fr $(PACKAGE_NAME).Rcheck
