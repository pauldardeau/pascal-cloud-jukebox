# pascal-cloud-jukebox

This is the Pascal port of cloud-jukebox. This project is written for use with Free Pascal compiler (fpc). It can be compiled and run (tested) on macOS, Windows 10, and Linux.

Unlike the python, go, and c# implementations, this one interfaces with S3 differently. Instead of using a native (same language SDK), it interfaces with S3 by running simple shell scripts. These scripts are found in the 'scripts' directory.
