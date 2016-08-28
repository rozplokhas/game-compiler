#include <stdio.h>
 
int main(void) {
    int acc = 0;
    static int mem[18];
    goto qc;

qc:
    goto qd;
nd:
    goto qh;
nh:
    goto nc;
qge:
    goto qi;
ni:
    goto nge;
qga:
    goto qj;
nj:
    goto nga;
qk:
    goto qu;
nu:
    goto nk;
qe:
    goto qr;
nr:
    goto ne;
qd:
    goto qe;
ne:
    goto nd;
qi:
    goto ql;
nl:
    goto ni;
qj:
    goto qm;
nm:
    goto nj;
qh:
    goto qk;
nk:
    goto nh;
ql:
    acc = 2;
    goto nl;
qm:
    acc = 10;
    goto nm;
qr:
    acc = 2;
    goto nr;
qu:
    goto qx;
nx:
    goto qfq;
nfq:
    goto nu;
qff:
    goto qgi;
ngi:
    goto nff;
qgj:
    goto qfg;
nfg:
    goto ngj;
qfh:
    goto qgk;
ngk:
    goto nfh;
qfi:
    goto qga;
nga:
    goto nfi;
qfj:
    mem[1] = 0;
    goto qgf;
qfu:
    mem[1] = 1;
    goto qgf;
ngf:
    if (mem[1])
        goto nfu;
    else
        goto nfj;
nfk:
    mem[1] = 0;
    goto ngg;
nfv:
    mem[1] = 1;
    goto ngg;
qgg:
    if (mem[1])
        goto qfv;
    else
        goto qfk;
qfl:
    mem[1] = 0;
    goto qgh;
qfw:
    mem[1] = 1;
    goto qgh;
ngh:
    if (mem[1])
        goto nfw;
    else
        goto nfl;
qfm:
    goto qge;
nge:
    goto nfm;
qx:
    goto qaa;
naa:
    goto qbq;
nbq:
    goto nx;
qbh:
    mem[2] = 0;
    goto qff;
qey:
    mem[2] = 1;
    goto qff;
nff:
    if (mem[2])
        goto ney;
    else
        goto nbh;
nbi:
    mem[2] = 0;
    goto nfg;
nez:
    mem[2] = 1;
    goto nfg;
qfg:
    if (mem[2])
        goto qez;
    else
        goto qbi;
qbj:
    mem[2] = 0;
    goto qfh;
qfa:
    mem[2] = 1;
    goto qfh;
nfh:
    if (mem[2])
        goto nfa;
    else
        goto nbj;
qbk:
    goto qfi;
nfi:
    goto nbk;
qbl:
    mem[2] = 0;
    goto qfj;
qfb:
    mem[2] = 1;
    goto qfj;
nfj:
    if (mem[2])
        goto nfb;
    else
        goto nbl;
nbm:
    mem[2] = 0;
    goto nfk;
nfc:
    mem[2] = 1;
    goto nfk;
qfk:
    if (mem[2])
        goto qfc;
    else
        goto qbm;
qbn:
    mem[2] = 0;
    goto qfl;
qfd:
    mem[2] = 1;
    goto qfl;
nfl:
    if (mem[2])
        goto nfd;
    else
        goto nbn;
qfe:
    goto qfm;
nfm:
    goto nfe;
qaa:
    goto qaf;
naf:
    goto qaw;
naw:
    goto naa;
qao:
    goto qbh;
nbh:
    goto nao;
qbi:
    goto qap;
nap:
    goto nbi;
qaq:
    goto qbj;
nbj:
    goto naq;
qar:
    goto qbk;
nbk:
    goto nar;
qbe:
    goto qbl;
nbl:
    goto nbe;
qbm:
    goto qbf;
nbf:
    goto nbm;
qbg:
    goto qbn;
nbn:
    goto nbg;
nag:
// dead-end
qaf:
    goto qam;
nam:
    mem[4] = acc;
    goto qai;
qah:
    acc = mem[4];
    goto nah;
nai:
    goto naf;
qaj:
    goto qao;
nao:
    goto naj;
qap:
    goto qak;
nak:
    goto nap;
qal:
    goto qaq;
naq:
    goto nal;
qan:
    goto qar;
nar:
    goto nan;
qag:
    goto qaj;
naj:
    goto nag;
qak:
    goto qah;
nah:
    goto nak;
qai:
    goto qal;
nal:
    goto nai;
qam:
    goto qan;
nan:
    goto nam;
nax:
// dead-end
qaw:
    goto qbd;
nbd:
    mem[6] = acc;
    goto qaz;
qay:
    acc = mem[6];
    goto nay;
naz:
    goto naw;
qba:
    goto qbe;
nbe:
    goto nba;
qbf:
    goto qbb;
nbb:
    goto nbf;
qbc:
    goto qbg;
nbg:
    goto nbc;
qax:
    goto qba;
nba:
    goto nax;
qbb:
    goto qay;
nay:
    goto nbb;
qaz:
    goto qbc;
nbc:
    goto naz;
qbd:
    acc = 1;
    goto nbd;
qbq:
    goto qbu;
nbu:
    if (acc)
        goto qcd;
    else
        goto nbq;
ncd:
    goto qbu;
qby:
    mem[7] = 0;
    goto qey;
qer:
    mem[7] = 1;
    goto qey;
ney:
    if (mem[7])
        goto ner;
    else
        goto nby;
nbz:
    mem[7] = 0;
    goto nez;
nes:
    mem[7] = 1;
    goto nez;
qez:
    if (mem[7])
        goto qes;
    else
        goto qbz;
qca:
    mem[7] = 0;
    goto qfa;
qet:
    mem[7] = 1;
    goto qfa;
nfa:
    if (mem[7])
        goto net;
    else
        goto nca;
qeu:
    goto qfb;
nfb:
    goto neu;
qfc:
    goto qev;
nev:
    goto nfc;
qew:
    goto qfd;
nfd:
    goto new;
qex:
    goto qfe;
nfe:
    goto nex;
nbx:
qbw:
// dead-end
qbu:
    goto qbv;
nbv:
    goto nbu;
qbv:
    goto qby;
nby:
    goto nbv;
qbz:
    goto qbw;
nbw:
    goto nbz;
qbx:
    goto qca;
nca:
    goto nbx;
qcd:
    goto qci;
nci:
    goto qdq;
ndq:
    goto ncd;
qeo:
    goto qer;
ner:
    goto neo;
qes:
    goto qep;
nep:
    goto nes;
qeq:
    goto qet;
net:
    goto neq;
qdi:
    goto qeu;
neu:
    goto ndi;
qev:
    goto qdj;
ndj:
    goto nev;
qdk:
    goto qew;
new:
    goto ndk;
qdl:
    goto qex;
nex:
    goto ndl;
ncj:
// dead-end
qci:
    goto qcr;
ncr:
    mem[9] = acc;
    goto qcl;
qck:
    acc = mem[9];
    goto nck;
ncl:
    goto nci;
qcm:
    mem[10] = 0;
    goto qdi;
qde:
    mem[10] = 1;
    goto qdi;
ndi:
    if (mem[10])
        goto nde;
    else
        goto ncm;
ncn:
    mem[10] = 0;
    goto ndj;
ndf:
    mem[10] = 1;
    goto ndj;
qdj:
    if (mem[10])
        goto qdf;
    else
        goto qcn;
qco:
    mem[10] = 0;
    goto qdk;
qdg:
    mem[10] = 1;
    goto qdk;
ndk:
    if (mem[10])
        goto ndg;
    else
        goto nco;
qdh:
    goto qdl;
ndl:
    goto ndh;
qcj:
    goto qcm;
ncm:
    goto ncj;
qcn:
    goto qck;
nck:
    goto ncn;
qcl:
    goto qco;
nco:
    goto ncl;
qcr:
    goto qcv;
ncv:
    mem[11] = acc;
    goto qdc;
ndc:
    acc = mem[11] * acc;
    goto ncr;
qcz:
    goto qde;
nde:
    goto ncz;
qdf:
    goto qda;
nda:
    goto ndf;
qdb:
    goto qdg;
ndg:
    goto ndb;
qdd:
    goto qdh;
ndh:
    goto ndd;
ncy:
qcx:
// dead-end
qcv:
    goto qcw;
ncw:
    goto ncv;
qcw:
    goto qcz;
ncz:
    goto ncw;
qda:
    goto qcx;
ncx:
    goto nda;
qcy:
    goto qdb;
ndb:
    goto ncy;
qdc:
    goto qdd;
ndd:
    goto ndc;
ndr:
// dead-end
qdq:
    goto qdz;
ndz:
    mem[13] = acc;
    goto qdt;
qds:
    acc = mem[13];
    goto nds;
ndt:
    goto ndq;
qdu:
    mem[14] = 0;
    goto qeo;
qel:
    mem[14] = 1;
    goto qeo;
neo:
    if (mem[14])
        goto nel;
    else
        goto ndu;
ndv:
    mem[14] = 0;
    goto nep;
nem:
    mem[14] = 1;
    goto nep;
qep:
    if (mem[14])
        goto qem;
    else
        goto qdv;
qdw:
    mem[14] = 0;
    goto qeq;
qen:
    mem[14] = 1;
    goto qeq;
neq:
    if (mem[14])
        goto nen;
    else
        goto ndw;
qdr:
    goto qdu;
ndu:
    goto ndr;
qdv:
    goto qds;
nds:
    goto ndv;
qdt:
    goto qdw;
ndw:
    goto ndt;
qdz:
    goto qed;
ned:
    mem[15] = acc;
    goto qek;
nek:
    acc = mem[15] - acc;
    goto ndz;
qeh:
    goto qel;
nel:
    goto neh;
qem:
    goto qei;
nei:
    goto nem;
qej:
    goto qen;
nen:
    goto nej;
neg:
qef:
// dead-end
qed:
    goto qee;
nee:
    goto ned;
qee:
    goto qeh;
neh:
    goto nee;
qei:
    goto qef;
nef:
    goto nei;
qeg:
    goto qej;
nej:
    goto neg;
qek:
    acc = 1;
    goto nek;
nft:
qfs:
// dead-end
qfq:
    goto qfr;
nfr:
    goto nfq;
qfr:
    goto qfu;
nfu:
    goto nfr;
qfv:
    goto qfs;
nfs:
    goto nfv;
qft:
    goto qfw;
nfw:
    goto nft;
qgf:
    acc = mem[16];
    goto ngf;
qgh:
    goto qgg;
ngg:
    mem[16] = acc;
    goto ngh;
qgi:
    acc = mem[17];
    goto ngi;
qgk:
    goto qgj;
ngj:
    mem[17] = acc;
    goto ngk;

nc:
    printf("%d\n", acc);
}
