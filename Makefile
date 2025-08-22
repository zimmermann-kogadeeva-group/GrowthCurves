
URL = docker://registry.git.embl.de/grp-zimmermann-kogadeeva/growthcurves
VERSION = 0.1.4

all: growthcurves_${VERSION}.sif

growthcurves_${VERSION}.sif: growthcurves
	sudo apptainer build $@ $< 

growthcurves: 
	apptainer build --sandbox $@ ${URL}:${VERSION}

clean:
	rm -rf *.sif

