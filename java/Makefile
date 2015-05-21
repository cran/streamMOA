SOURCES = StreamMOA.java
OBJECTS = $(SOURCES:.java=.class)
JARS = StreamMOA.jar

JAVAC = javac
JAR = jar
LIBS = ../inst/java/moa.jar

.SUFFIXES: .class .java

.java.class:
	$(JAVAC) -cp $(LIBS) $<

all: compile install clean

$(JARS): $(OBJECTS)
	$(JAR) cvf $@ $(OBJECTS) 

compile: $(JARS)

install:
	@cp $(JARS) ../inst/java

clean:
	-@rm -f $(OBJECTS) $(JARS)
