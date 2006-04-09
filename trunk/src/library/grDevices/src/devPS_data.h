/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005  R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/*
 * Support CID Font family list.
 *
       cidfamily        postscript              pdf
       --------------------------------------------------------------
       Japan1           HeiseiKakuGo-W5         HeiseiKakuGo-W5-Acro
       Japan1HeiMin     HeiseiMin-W3            HeiseiMin-W3-Acro
       Japan1GothicBBB  GothicBBB-Medium        GothicBBB-Medium
       Japan1Ryumin     Ryumin-Light            Ryumin-Light
       Korea1           Baekmuk-Batang          HYGothic-Medium-Acro
       Korea1deb        Batang-Regular          HYGothic-Medium-Acro
       CNS1             MOESung-Regular         MHei-Medium-Acro
       GB1              BousungEG-Light-GB      STSong-Light-Acro

       [BDR comment, 

        Adobe's UNIX fonts packs have

	 chsfont.tar.gz has STSongStd-Light-Acro.otf
	 chtfont.tar.gz has MSungStd-Light-Acro.otf
	 jpnfont.tar.gz  has KozMinPro-Regular-Acro.otf
	 korfont.tar.gz has HYSMyeongJoStd-Medium-Acro.otf

       and Windows versions have

	 AdobeMingStd-Light.otf
	 AdobeMyungjoStd-Medium.otf (Korean)
	 AdobeSongStd-Light.otf
	 KozMinProVI-Regular.otf    (Japanese)
	 KozGoPro-Medium.otf        (Japanese)

       gs installed on Windows (and hence GSView) will optionally have

         MingLiU PMingLiU (CNS1)
         NSimSum SimHei (GB1)
         MS-Gothic MS-Mincho MS-PGothic MS-PMincho (Japan1)
         Batang BatangChe Dotum DotumChe Gulim GulimChe Gungsuh (Korea1)

       via WIndows ttf fonts.

       ftp://ftp.oreilly.com/pub/examples/nutshell/cjkv/adobe/samples/
       provides PS Type 1 fonts (no extensions) and corresponding .afm
       files (but not the format expected with no CH field).

	 Munhwa-Regular            (Adobe-Korea1-0 subset: 8193 CIDs)
	 Munhwa-Bold               (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaGothic-Regular      (Adobe-Korea1-0 subset: 8193 CIDs)
	 MunhwaGothic-Bold         (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaGungSeo-Bold        (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaGungSeo-Light       (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaGungSeoHeulim-Bold  (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaGungSeoHeulim-Light (Adobe-Korea1-0 subset: 2549 CIDs)
	 MunhwaHoonMin-Regular     (Adobe-Korea1-0 subset: 2549 CIDs)
	 WadaMin-Regular           (Adobe-Japan1-1 subset: 6998 CIDs
	 WadaMin-Bold              (Adobe-Japan1-1 subset: 6996 CIDs)
	 WadaGo-Bold               (Adobe-Japan1-1 subset: 6998 CIDs)
	 WadaMaruGo-Regular        (Adobe-Japan1-1 subset: 6998 CIDs)
	 WadaMin-RegularH          (Adobe-Japan2-0 complete: 6068 CIDs)
	 WadaMaruGo-RegularH       (Adobe-Japan2-0 complete: 6068 CIDs)
	 MOESung-Regular           (Adobe-CNS1-0 subset: 13699 CIDs)
	 MOEKai-Regular            (Adobe-CNS1-0 subset: 13699 CIDs)
      ]


       Debian
       ------------------
       font not found case.
       /etc/defoma/hints/ttf-*.hints
       edit defoma hints file.
 
       +++ /etc/defoma/hints/ttf-sazanami-gothic.hints
           Family = SazanamiGothic
           FontName = SazanamiGothic-Regular
       +   Alias = HeiseiKakuGo-W5
           Encoding = Unicode
           Location = Japanese
 
       +++ /etc/defoma/hints/ttf-sazanami-mincho.hints
           Family = SazanamiMincho
           FontName = SazanamiMincho-Regular
       +   Alias = HeiseiMin-W3
           Encoding = Unicode
           Location = Japanese
 
       +++ /etc/defoma/hints/ttf-baekmuk.hints
           Family = Batang
           FontName = Batang-Regular
       +   Alias = Baekmuk-Batang
           Encoding = Unicode
           Location = Korean

       Try this command.
       $ sudo defoma-font reregister-all \
       >     /etc/defoma/hints/ttf-sazanami-gothic.hints
 *
 *
 */

