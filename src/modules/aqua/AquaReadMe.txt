ReadMe file for build, install and execute the AQUA - Darwin R


The final installation should have the following structure

To build Darwin R with experimental Aqua GUI you have to configure as follows

1)

./configure --with-aqua --without-x

(I'm not sure it --without-x is necessary)


To install "RAqua" in the Applications folder just execute one time

2)

./AquaInstall

from this directory

to launch "RAqua" type

3)

sh /Applications/RAqua.app/Contents/MacOS/R --gui-aqua 


you can enjoy the Quartz device (using "quartz()") and the new environment 
browser by typing "browseEnv(html=FALSE)" from the shell. To refresh the 
browser type again the same command.

It is a first experimental release for R 1.6.0.

Sep 7 2002
Stefano M. Iacus


