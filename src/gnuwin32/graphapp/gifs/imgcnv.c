#include "../graphapp.h"

int AppMain(int argc,char **argv) {
  image img;
  img = loadimage(argv[1]);
  saveimage(img,argv[2]);
}
