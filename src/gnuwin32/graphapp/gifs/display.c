#include "../graphapp.h"

int AppMain(int argc,char **argv) {
  image img = loadimage(argv[1]);
  rect ri = rect(0,0,img->width,img->height);
  rect rw = rect(0,0,4*img->width+6,img->height+4);
  window w = newwindow(argv[1],rw,StandardWindow|Centered);
  rw.x = 2;
  rw.y = 2;
  rw.width = img->width;
  rw.height = img->height;
  drawimage(img,rw,ri);
  rw.x += (ri.width+1);
  drawgreyscale(img,rw,ri);
  rw.x += (ri.width+1);
  drawdarker(img,rw,ri);
  rw.x += (ri.width+1);
  drawbrighter(img,rw,ri);
  gamainloop();
}
