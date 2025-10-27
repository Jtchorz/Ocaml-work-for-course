.PHONY: all build clean

# Change these
EXE = hello
MAIN_CLASS = Hello


MAVEN_DIR = .m2
MVN = mvn "-Dmaven.repo.local=$(MAVEN_DIR)" \
          "-Did2202.build.mainClass=$(MAIN_CLASS)"

all: build

build:
	$(MVN) package
	echo '#!/usr/bin/env bash' > $(EXE)
	echo 'exec java -jar target/id2202-solution-1.0-SNAPSHOT-shaded.jar "$$@"' >> $(EXE)
	chmod +x $(EXE)

test:
	$(MVN) test

clean:
	$(MVN) clean
	rm -f $(EXE)
