# Copyright Paul Dardeau, 2023

EXE_NAME = pas-cloud-jukebox

OBJS =  ArgumentParser.o \
FileMetadata.o \
IniReader.o \
JBPlatform.o \
JBSysUtils.o \
JBUtils.o \
JukeboxDB.o \
JukeboxOptions.o \
KeyValuePairs.o \
PropertyList.o \
PropertySet.o \
PropertyValue.o \
StorageSystem.o \
S3ExtStorageSystem.o \
FSStorageSystem.o \
SongDownloader.o \
SongDownloaderThread.o \
SongMetadata.o \
StringSet.o \
AbstractJukebox.o \
Jukebox.o \
JukeboxMain.o


all : $(EXE_NAME)

clean :
	rm -f *.o
	rm -f $(EXE_NAME)

$(EXE_NAME) : $(OBJS)
	fpc pas-cloud-jukebox.pas

%.o : %.pas
	fpc $<
