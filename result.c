#include <stdio.h>
 
int main(void) {
    int acc = 0;
    static int mem[17];
    goto qc;

qfu:
    goto qg;
ng:
    goto nfu;
qfq:
    goto qh;
nh:
    goto nfq;
qc:
    goto qk;
nk:
    goto nc;
qg:
    acc = 2;
    goto ng;
qh:
    acc = 10;
    goto nh;
qk:
    goto qn;
nn:
    goto qfg;
nfg:
    goto nk;
qev:
    goto qfy;
nfy:
    goto nev;
qfz:
    goto qew;
new:
    goto nfz;
qex:
    goto qga;
nga:
    goto nex;
qey:
    goto qfq;
nfq:
    goto ney;
qez:
    mem[0] = 0;
    goto qfv;
qfk:
    mem[0] = 1;
    goto qfv;
nfv:
    if (mem[0])
        goto nfk;
    else
        goto nez;
nfa:
    mem[0] = 0;
    goto nfw;
nfl:
    mem[0] = 1;
    goto nfw;
qfw:
    if (mem[0])
        goto qfl;
    else
        goto qfa;
qfb:
    mem[0] = 0;
    goto qfx;
qfm:
    mem[0] = 1;
    goto qfx;
nfx:
    if (mem[0])
        goto nfm;
    else
        goto nfb;
qfc:
    goto qfu;
nfu:
    goto nfc;
qn:
    goto qq;
nq:
    goto qbg;
nbg:
    goto nn;
qax:
    mem[1] = 0;
    goto qev;
qeo:
    mem[1] = 1;
    goto qev;
nev:
    if (mem[1])
        goto neo;
    else
        goto nax;
nay:
    mem[1] = 0;
    goto new;
nep:
    mem[1] = 1;
    goto new;
qew:
    if (mem[1])
        goto qep;
    else
        goto qay;
qaz:
    mem[1] = 0;
    goto qex;
qeq:
    mem[1] = 1;
    goto qex;
nex:
    if (mem[1])
        goto neq;
    else
        goto naz;
qba:
    goto qey;
ney:
    goto nba;
qbb:
    mem[1] = 0;
    goto qez;
qer:
    mem[1] = 1;
    goto qez;
nez:
    if (mem[1])
        goto ner;
    else
        goto nbb;
nbc:
    mem[1] = 0;
    goto nfa;
nes:
    mem[1] = 1;
    goto nfa;
qfa:
    if (mem[1])
        goto qes;
    else
        goto qbc;
qbd:
    mem[1] = 0;
    goto qfb;
qet:
    mem[1] = 1;
    goto qfb;
nfb:
    if (mem[1])
        goto net;
    else
        goto nbd;
qeu:
    goto qfc;
nfc:
    goto neu;
qq:
    goto qv;
nv:
    goto qam;
nam:
    goto nq;
qae:
    goto qax;
nax:
    goto nae;
qay:
    goto qaf;
naf:
    goto nay;
qag:
    goto qaz;
naz:
    goto nag;
qah:
    goto qba;
nba:
    goto nah;
qau:
    goto qbb;
nbb:
    goto nau;
qbc:
    goto qav;
nav:
    goto nbc;
qaw:
    goto qbd;
nbd:
    goto naw;
nw:
// dead-end
qv:
    goto qac;
nac:
    mem[3] = acc;
    goto qy;
qx:
    acc = mem[3];
    goto nx;
ny:
    goto nv;
qz:
    goto qae;
nae:
    goto nz;
qaf:
    goto qaa;
naa:
    goto naf;
qab:
    goto qag;
nag:
    goto nab;
qad:
    goto qah;
nah:
    goto nad;
qw:
    goto qz;
nz:
    goto nw;
qaa:
    goto qx;
nx:
    goto naa;
qy:
    goto qab;
nab:
    goto ny;
qac:
    goto qad;
nad:
    goto nac;
nan:
// dead-end
qam:
    goto qat;
nat:
    mem[5] = acc;
    goto qap;
qao:
    acc = mem[5];
    goto nao;
nap:
    goto nam;
qaq:
    goto qau;
nau:
    goto naq;
qav:
    goto qar;
nar:
    goto nav;
qas:
    goto qaw;
naw:
    goto nas;
qan:
    goto qaq;
naq:
    goto nan;
qar:
    goto qao;
nao:
    goto nar;
qap:
    goto qas;
nas:
    goto nap;
qat:
    acc = 1;
    goto nat;
qbg:
    goto qbk;
nbk:
    if (acc)
        goto qbt;
    else
        goto nbg;
nbt:
    goto qbk;
qbo:
    mem[6] = 0;
    goto qeo;
qeh:
    mem[6] = 1;
    goto qeo;
neo:
    if (mem[6])
        goto neh;
    else
        goto nbo;
nbp:
    mem[6] = 0;
    goto nep;
nei:
    mem[6] = 1;
    goto nep;
qep:
    if (mem[6])
        goto qei;
    else
        goto qbp;
qbq:
    mem[6] = 0;
    goto qeq;
qej:
    mem[6] = 1;
    goto qeq;
neq:
    if (mem[6])
        goto nej;
    else
        goto nbq;
qek:
    goto qer;
ner:
    goto nek;
qes:
    goto qel;
nel:
    goto nes;
qem:
    goto qet;
net:
    goto nem;
qen:
    goto qeu;
neu:
    goto nen;
nbn:
qbm:
// dead-end
qbk:
    goto qbl;
nbl:
    goto nbk;
qbl:
    goto qbo;
nbo:
    goto nbl;
qbp:
    goto qbm;
nbm:
    goto nbp;
qbn:
    goto qbq;
nbq:
    goto nbn;
qbt:
    goto qby;
nby:
    goto qdg;
ndg:
    goto nbt;
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
qcy:
    goto qek;
nek:
    goto ncy;
qel:
    goto qcz;
ncz:
    goto nel;
qda:
    goto qem;
nem:
    goto nda;
qdb:
    goto qen;
nen:
    goto ndb;
nbz:
// dead-end
qby:
    goto qch;
nch:
    mem[8] = acc;
    goto qcb;
qca:
    acc = mem[8];
    goto nca;
ncb:
    goto nby;
qcc:
    mem[9] = 0;
    goto qcy;
qcu:
    mem[9] = 1;
    goto qcy;
ncy:
    if (mem[9])
        goto ncu;
    else
        goto ncc;
ncd:
    mem[9] = 0;
    goto ncz;
ncv:
    mem[9] = 1;
    goto ncz;
qcz:
    if (mem[9])
        goto qcv;
    else
        goto qcd;
qce:
    mem[9] = 0;
    goto qda;
qcw:
    mem[9] = 1;
    goto qda;
nda:
    if (mem[9])
        goto ncw;
    else
        goto nce;
qcx:
    goto qdb;
ndb:
    goto ncx;
qbz:
    goto qcc;
ncc:
    goto nbz;
qcd:
    goto qca;
nca:
    goto ncd;
qcb:
    goto qce;
nce:
    goto ncb;
qch:
    goto qcl;
ncl:
    mem[10] = acc;
    goto qcs;
ncs:
    acc = mem[10] * acc;
    goto nch;
qcp:
    goto qcu;
ncu:
    goto ncp;
qcv:
    goto qcq;
ncq:
    goto ncv;
qcr:
    goto qcw;
ncw:
    goto ncr;
qct:
    goto qcx;
ncx:
    goto nct;
nco:
qcn:
// dead-end
qcl:
    goto qcm;
ncm:
    goto ncl;
qcm:
    goto qcp;
ncp:
    goto ncm;
qcq:
    goto qcn;
ncn:
    goto ncq;
qco:
    goto qcr;
ncr:
    goto nco;
qcs:
    goto qct;
nct:
    goto ncs;
ndh:
// dead-end
qdg:
    goto qdp;
ndp:
    mem[12] = acc;
    goto qdj;
qdi:
    acc = mem[12];
    goto ndi;
ndj:
    goto ndg;
qdk:
    mem[13] = 0;
    goto qee;
qeb:
    mem[13] = 1;
    goto qee;
nee:
    if (mem[13])
        goto neb;
    else
        goto ndk;
ndl:
    mem[13] = 0;
    goto nef;
nec:
    mem[13] = 1;
    goto nef;
qef:
    if (mem[13])
        goto qec;
    else
        goto qdl;
qdm:
    mem[13] = 0;
    goto qeg;
qed:
    mem[13] = 1;
    goto qeg;
neg:
    if (mem[13])
        goto ned;
    else
        goto ndm;
qdh:
    goto qdk;
ndk:
    goto ndh;
qdl:
    goto qdi;
ndi:
    goto ndl;
qdj:
    goto qdm;
ndm:
    goto ndj;
qdp:
    goto qdt;
ndt:
    mem[14] = acc;
    goto qea;
nea:
    acc = mem[14] - acc;
    goto ndp;
qdx:
    goto qeb;
neb:
    goto ndx;
qec:
    goto qdy;
ndy:
    goto nec;
qdz:
    goto qed;
ned:
    goto ndz;
ndw:
qdv:
// dead-end
qdt:
    goto qdu;
ndu:
    goto ndt;
qdu:
    goto qdx;
ndx:
    goto ndu;
qdy:
    goto qdv;
ndv:
    goto ndy;
qdw:
    goto qdz;
ndz:
    goto ndw;
qea:
    acc = 1;
    goto nea;
nfj:
qfi:
// dead-end
qfg:
    goto qfh;
nfh:
    goto nfg;
qfh:
    goto qfk;
nfk:
    goto nfh;
qfl:
    goto qfi;
nfi:
    goto nfl;
qfj:
    goto qfm;
nfm:
    goto nfj;
qfv:
    acc = mem[15];
    goto nfv;
qfx:
    goto qfw;
nfw:
    mem[15] = acc;
    goto nfx;
qfy:
    acc = mem[16];
    goto nfy;
qga:
    goto qfz;
nfz:
    mem[16] = acc;
    goto nga;

nc:
    printf("%d\n", acc);
}
