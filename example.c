#include <stdio.h>
 
int main(void) {
    int acc = 0;
    static int mem[48];
    goto qc;

qc:
    goto qf;
nf:
    goto qbe;
nbe:
    goto nc;
qbk:
    goto qcx;
ncx:
    goto nbk;
qcy:
    goto qbl;
nbl:
    goto ncy;
qbm:
    goto qcw;
ncw:
    goto nbm;
qcp:
    goto qay;
nay:
    goto ncp;
qaz:
    goto qch;
nch:
    goto naz;
qls:
    goto qbf;
nbf:
    goto nls;
qbg:
    goto qdc;
ndc:
    goto nbg;
nba:
    mem[0] = 0;
    goto nso;
nbq:
    mem[0] = 1;
    goto nso;
qso:
    if (mem[0])
        goto qbq;
    else
        goto qba;
nbb:
    mem[0] = 0;
    goto nsk;
nbr:
    mem[0] = 1;
    goto nsk;
qsk:
    if (mem[0])
        goto qbr;
    else
        goto qbb;
qbc:
    mem[0] = 0;
    goto qme;
qbs:
    mem[0] = 1;
    goto qme;
nme:
    if (mem[0])
        goto nbs;
    else
        goto nbc;
qf:
    goto qh;
nh:
    goto qaa;
naa:
    goto nf;
ni:
    mem[1] = 0;
    goto nay;
nab:
    mem[1] = 1;
    goto nay;
qay:
    if (mem[1])
        goto qab;
    else
        goto qi;
qj:
    mem[1] = 0;
    goto qaz;
qac:
    mem[1] = 1;
    goto qaz;
naz:
    if (mem[1])
        goto nac;
    else
        goto nj;
qba:
    goto qw;
nw:
    goto nba;
qbb:
    goto qx;
nx:
    goto nbb;
qy:
    goto qbc;
nbc:
    goto ny;
qi:
    goto qm;
nm:
    goto ni;
qh:
    goto qj;
nj:
    goto nh;
qm:
    goto qp;
np:
    mem[2] = acc;
    goto qv;
nv:
    acc = mem[2] == acc;
    goto nm;
qw:
    goto qq;
nq:
    goto nw;
qx:
    goto qr;
nr:
    goto nx;
qs:
    goto qy;
ny:
    goto ns;
qq:
    goto qt;
nt:
    goto nq;
qr:
    goto qu;
nu:
    goto nr;
qp:
    goto qs;
ns:
    goto np;
qt:
    acc = 2;
    goto nt;
qu:
    acc = 10;
    goto nu;
qv:
    acc = 1024;
    goto nv;
qab:
    goto qaf;
naf:
    goto nab;
qaa:
    goto qac;
nac:
    goto naa;
qaf:
    goto qai;
nai:
    mem[3] = acc;
    goto qax;
nax:
    acc = mem[3] == acc;
    goto naf;
qai:
    goto qal;
nal:
    mem[4] = acc;
    goto qaw;
naw:
    acc = mem[4] - acc;
    goto nai;
qal:
    goto qao;
nao:
    mem[5] = acc;
    goto qav;
nav:
    acc = mem[5] - acc;
    goto nal;
qao:
    goto qap;
nap:
    mem[6] = acc;
    goto qas;
nas:
    acc = mem[6] - acc;
    goto nao;
qap:
    acc = 4;
    goto nap;
qas:
    goto qat;
nat:
    mem[7] = acc;
    goto qau;
nau:
    acc = mem[7] * acc;
    goto nas;
qat:
    acc = 2;
    goto nat;
qau:
    acc = 0;
    goto nau;
qav:
    acc = 1;
    goto nav;
qaw:
    acc = 1;
    goto naw;
qax:
    acc = 2;
    goto nax;
qbf:
    goto qbj;
nbj:
    goto nbf;
qbe:
    goto qbg;
nbg:
    goto nbe;
qbo:
    goto qbk;
nbk:
    goto nbo;
qbl:
    goto qbp;
nbp:
    goto nbl;
qbj:
    goto qbm;
nbm:
    goto nbj;
qbq:
    goto qbt;
nbt:
    goto nbq;
qbr:
    goto qbo;
nbo:
    goto nbr;
qbp:
    goto qbs;
nbs:
    goto nbp;
qbt:
    acc = 4;
    goto nbt;
qch:
    goto qci;
nci:
    if (acc)
        goto qck;
    else
        goto qcm;
nck:
    goto nch;
ncm:
    goto nch;
qcn:
    goto qcs;
ncs:
    goto ncn;
qcl:
    goto qcp;
ncp:
    goto ncl;
qcj:
    goto qcl;
ncl:
    goto ncj;
qci:
    goto qcj;
ncj:
    goto nci;
qck:
    acc = 0;
    goto nck;
qcm:
    goto qcn;
ncn:
    goto ncm;
qcs:
    goto qct;
nct:
    if (acc)
        goto qcu;
    else
        goto ncs;
ncu:
    goto qct;
qct:
    acc = 1;
    goto nct;
qcu:
    acc = 0;
    goto ncu;
qcx:
    goto qcz;
ncz:
    goto ncx;
qcw:
    goto qcy;
ncy:
    goto ncw;
qcz:
    acc = 2;
    goto ncz;
qdc:
    goto qdf;
ndf:
    goto qlc;
nlc:
    goto ndc;
qkp:
    mem[9] = 0;
    goto qlz;
qlg:
    mem[9] = 1;
    goto qlz;
nlz:
    if (mem[9])
        goto nlg;
    else
        goto nkp;
nkq:
    mem[9] = 0;
    goto nma;
nlh:
    mem[9] = 1;
    goto nma;
qma:
    if (mem[9])
        goto qlh;
    else
        goto qkq;
qkr:
    mem[9] = 0;
    goto qmb;
qli:
    mem[9] = 1;
    goto qmb;
nmb:
    if (mem[9])
        goto nli;
    else
        goto nkr;
qks:
    goto qlw;
nlw:
    goto nks;
qlx:
    goto qkt;
nkt:
    goto nlx;
qku:
    goto qly;
nly:
    goto nku;
qkv:
    goto qlt;
nlt:
    goto nkv;
qlu:
    goto qkw;
nkw:
    goto nlu;
qkx:
    goto qlv;
nlv:
    goto nkx;
qky:
    goto qls;
nls:
    goto nky;
qdf:
    goto qdk;
ndk:
    goto qdx;
ndx:
    goto ndf;
qkf:
    goto qkp;
nkp:
    goto nkf;
qkq:
    goto qkg;
nkg:
    goto nkq;
qkh:
    goto qkr;
nkr:
    goto nkh;
qds:
    mem[10] = 0;
    goto qks;
qki:
    mem[10] = 1;
    goto qks;
nks:
    if (mem[10])
        goto nki;
    else
        goto nds;
ndt:
    mem[10] = 0;
    goto nkt;
nkj:
    mem[10] = 1;
    goto nkt;
qkt:
    if (mem[10])
        goto qkj;
    else
        goto qdt;
qdu:
    mem[10] = 0;
    goto qku;
qkk:
    mem[10] = 1;
    goto qku;
nku:
    if (mem[10])
        goto nkk;
    else
        goto ndu;
qkl:
    goto qkv;
nkv:
    goto nkl;
qkw:
    goto qkm;
nkm:
    goto nkw;
qkn:
    goto qkx;
nkx:
    goto nkn;
qko:
    goto qky;
nky:
    goto nko;
ndl:
// dead-end
qdk:
    goto qdr;
ndr:
    mem[11] = acc;
    goto qdn;
qdm:
    acc = mem[11];
    goto ndm;
ndn:
    goto ndk;
qdo:
    goto qds;
nds:
    goto ndo;
qdt:
    goto qdp;
ndp:
    goto ndt;
qdq:
    goto qdu;
ndu:
    goto ndq;
qdl:
    goto qdo;
ndo:
    goto ndl;
qdp:
    goto qdm;
ndm:
    goto ndp;
qdn:
    goto qdq;
ndq:
    goto ndn;
qdr:
    acc = 1;
    goto ndr;
qdx:
    goto qea;
nea:
    if (acc)
        goto qet;
    else
        goto ndx;
net:
    goto qea;
qjw:
    goto qkf;
nkf:
    goto njw;
qkg:
    goto qjx;
njx:
    goto nkg;
qjy:
    goto qkh;
nkh:
    goto njy;
qjz:
    goto qki;
nki:
    goto njz;
qkj:
    goto qka;
nka:
    goto nkj;
qkb:
    goto qkk;
nkk:
    goto nkb;
qen:
    mem[12] = 0;
    goto qkl;
qkc:
    mem[12] = 1;
    goto qkl;
nkl:
    if (mem[12])
        goto nkc;
    else
        goto nen;
neo:
    mem[12] = 0;
    goto nkm;
nkd:
    mem[12] = 1;
    goto nkm;
qkm:
    if (mem[12])
        goto qkd;
    else
        goto qeo;
qep:
    mem[12] = 0;
    goto qkn;
qke:
    mem[12] = 1;
    goto qkn;
nkn:
    if (mem[12])
        goto nke;
    else
        goto nep;
qeq:
    goto qko;
nko:
    goto neq;
qea:
    goto qee;
nee:
    mem[13] = acc;
    goto qel;
nel:
    acc = mem[13] != acc;
    goto nea;
qei:
    goto qen;
nen:
    goto nei;
qeo:
    goto qej;
nej:
    goto neo;
qek:
    goto qep;
nep:
    goto nek;
qem:
    goto qeq;
neq:
    goto nem;
neh:
qeg:
// dead-end
qee:
    goto qef;
nef:
    goto nee;
qef:
    goto qei;
nei:
    goto nef;
qej:
    goto qeg;
neg:
    goto nej;
qeh:
    goto qek;
nek:
    goto neh;
qel:
    goto qem;
nem:
    goto nel;
qet:
    goto qew;
new:
    goto qiv;
niv:
    goto net;
qil:
    goto qjw;
njw:
    goto nil;
qjx:
    goto qim;
nim:
    goto njx;
qin:
    goto qjy;
njy:
    goto nin;
qio:
    goto qjz;
njz:
    goto nio;
qka:
    goto qip;
nip:
    goto nka;
qiq:
    goto qkb;
nkb:
    goto niq;
qjt:
    goto qkc;
nkc:
    goto njt;
qkd:
    goto qju;
nju:
    goto nkd;
qjv:
    goto qke;
nke:
    goto njv;
qew:
    goto qfb;
nfb:
    goto qgv;
ngv:
    goto new;
qgl:
    mem[16] = 0;
    goto qil;
qif:
    mem[16] = 1;
    goto qil;
nil:
    if (mem[16])
        goto nif;
    else
        goto ngl;
ngm:
    mem[16] = 0;
    goto nim;
nig:
    mem[16] = 1;
    goto nim;
qim:
    if (mem[16])
        goto qig;
    else
        goto qgm;
qgn:
    mem[16] = 0;
    goto qin;
qih:
    mem[16] = 1;
    goto qin;
nin:
    if (mem[16])
        goto nih;
    else
        goto ngn;
qgo:
    mem[16] = 0;
    goto qio;
qii:
    mem[16] = 1;
    goto qio;
nio:
    if (mem[16])
        goto nii;
    else
        goto ngo;
ngp:
    mem[16] = 0;
    goto nip;
nij:
    mem[16] = 1;
    goto nip;
qip:
    if (mem[16])
        goto qij;
    else
        goto qgp;
qgq:
    mem[16] = 0;
    goto qiq;
qik:
    mem[16] = 1;
    goto qiq;
niq:
    if (mem[16])
        goto nik;
    else
        goto ngq;
nfc:
// dead-end
qfb:
    goto qfk;
nfk:
    mem[17] = acc;
    goto qfe;
qfd:
    acc = mem[17];
    goto nfd;
nfe:
    goto nfb;
qgf:
    goto qgl;
ngl:
    goto ngf;
qgm:
    goto qgg;
ngg:
    goto ngm;
qgh:
    goto qgn;
ngn:
    goto ngh;
qff:
    mem[18] = 0;
    goto qgo;
qgi:
    mem[18] = 1;
    goto qgo;
ngo:
    if (mem[18])
        goto ngi;
    else
        goto nff;
nfg:
    mem[18] = 0;
    goto ngp;
ngj:
    mem[18] = 1;
    goto ngp;
qgp:
    if (mem[18])
        goto qgj;
    else
        goto qfg;
qfh:
    mem[18] = 0;
    goto qgq;
qgk:
    mem[18] = 1;
    goto qgq;
ngq:
    if (mem[18])
        goto ngk;
    else
        goto nfh;
qfc:
    goto qff;
nff:
    goto nfc;
qfg:
    goto qfd;
nfd:
    goto nfg;
qfe:
    goto qfh;
nfh:
    goto nfe;
qfk:
    goto qfo;
nfo:
    mem[19] = acc;
    goto qfy;
nfy:
    acc = mem[19] + acc;
    goto nfk;
qfs:
    goto qgf;
ngf:
    goto nfs;
qgg:
    goto qft;
nft:
    goto ngg;
qfu:
    goto qgh;
ngh:
    goto nfu;
qgc:
    goto qgi;
ngi:
    goto ngc;
qgj:
    goto qgd;
ngd:
    goto ngj;
qge:
    goto qgk;
ngk:
    goto nge;
nfr:
qfq:
// dead-end
qfo:
    goto qfp;
nfp:
    goto nfo;
qfp:
    goto qfs;
nfs:
    goto nfp;
qft:
    goto qfq;
nfq:
    goto nft;
qfr:
    goto qfu;
nfu:
    goto nfr;
ngb:
qga:
// dead-end
qfy:
    goto qfz;
nfz:
    goto nfy;
qfz:
    goto qgc;
ngc:
    goto nfz;
qgd:
    goto qga;
nga:
    goto ngd;
qgb:
    goto qge;
nge:
    goto ngb;
ngw:
// dead-end
qgv:
    goto qhe;
nhe:
    mem[21] = acc;
    goto qgy;
qgx:
    acc = mem[21];
    goto ngx;
ngy:
    goto ngv;
qgz:
    mem[22] = 0;
    goto qif;
qhz:
    mem[22] = 1;
    goto qif;
nif:
    if (mem[22])
        goto nhz;
    else
        goto ngz;
nha:
    mem[22] = 0;
    goto nig;
nia:
    mem[22] = 1;
    goto nig;
qig:
    if (mem[22])
        goto qia;
    else
        goto qha;
qhb:
    mem[22] = 0;
    goto qih;
qib:
    mem[22] = 1;
    goto qih;
nih:
    if (mem[22])
        goto nib;
    else
        goto nhb;
qic:
    goto qii;
nii:
    goto nic;
qij:
    goto qid;
nid:
    goto nij;
qie:
    goto qik;
nik:
    goto nie;
qgw:
    goto qgz;
ngz:
    goto ngw;
qha:
    goto qgx;
ngx:
    goto nha;
qgy:
    goto qhb;
nhb:
    goto ngy;
qhe:
    goto qhi;
nhi:
    mem[23] = acc;
    goto qhs;
nhs:
    acc = mem[23] - acc;
    goto nhe;
qhw:
    goto qhz;
nhz:
    goto nhw;
qia:
    goto qhx;
nhx:
    goto nia;
qhy:
    goto qib;
nib:
    goto nhy;
qhm:
    goto qic;
nic:
    goto nhm;
qid:
    goto qhn;
nhn:
    goto nid;
qho:
    goto qie;
nie:
    goto nho;
nhl:
qhk:
// dead-end
qhi:
    goto qhj;
nhj:
    goto nhi;
qhj:
    goto qhm;
nhm:
    goto nhj;
qhn:
    goto qhk;
nhk:
    goto nhn;
qhl:
    goto qho;
nho:
    goto nhl;
nhv:
qhu:
// dead-end
qhs:
    goto qht;
nht:
    goto nhs;
qht:
    goto qhw;
nhw:
    goto nht;
qhx:
    goto qhu;
nhu:
    goto nhx;
qhv:
    goto qhy;
nhy:
    goto nhv;
niw:
// dead-end
qiv:
    goto qje;
nje:
    mem[25] = acc;
    goto qiy;
qix:
    acc = mem[25];
    goto nix;
niy:
    goto niv;
qiz:
    mem[26] = 0;
    goto qjt;
qjq:
    mem[26] = 1;
    goto qjt;
njt:
    if (mem[26])
        goto njq;
    else
        goto niz;
nja:
    mem[26] = 0;
    goto nju;
njr:
    mem[26] = 1;
    goto nju;
qju:
    if (mem[26])
        goto qjr;
    else
        goto qja;
qjb:
    mem[26] = 0;
    goto qjv;
qjs:
    mem[26] = 1;
    goto qjv;
njv:
    if (mem[26])
        goto njs;
    else
        goto njb;
qiw:
    goto qiz;
niz:
    goto niw;
qja:
    goto qix;
nix:
    goto nja;
qiy:
    goto qjb;
njb:
    goto niy;
qje:
    goto qji;
nji:
    mem[27] = acc;
    goto qjp;
njp:
    acc = mem[27] + acc;
    goto nje;
qjm:
    goto qjq;
njq:
    goto njm;
qjr:
    goto qjn;
njn:
    goto njr;
qjo:
    goto qjs;
njs:
    goto njo;
njl:
qjk:
// dead-end
qji:
    goto qjj;
njj:
    goto nji;
qjj:
    goto qjm;
njm:
    goto njj;
qjn:
    goto qjk;
njk:
    goto njn;
qjl:
    goto qjo;
njo:
    goto njl;
qjp:
    acc = 1;
    goto njp;
nlf:
qle:
// dead-end
qlc:
    goto qld;
nld:
    goto nlc;
qld:
    goto qlg;
nlg:
    goto nld;
qlh:
    goto qle;
nle:
    goto nlh;
qlf:
    goto qli;
nli:
    goto nlf;
qlt:
    acc = mem[28];
    goto nlt;
qlv:
    goto qlu;
nlu:
    mem[28] = acc;
    goto nlv;
qlw:
    acc = mem[29];
    goto nlw;
qly:
    goto qlx;
nlx:
    mem[29] = acc;
    goto nly;
qlz:
    acc = mem[30];
    goto nlz;
qmb:
    goto qma;
nma:
    mem[30] = acc;
    goto nmb;
qme:
    goto qmh;
nmh:
    goto qsa;
nsa:
    goto nme;
qrp:
    goto qss;
nss:
    goto nrp;
qst:
    goto qrq;
nrq:
    goto nst;
qrr:
    goto qsu;
nsu:
    goto nrr;
qrs:
    goto qsk;
nsk:
    goto nrs;
qrt:
    mem[31] = 0;
    goto qsp;
qse:
    mem[31] = 1;
    goto qsp;
nsp:
    if (mem[31])
        goto nse;
    else
        goto nrt;
nru:
    mem[31] = 0;
    goto nsq;
nsf:
    mem[31] = 1;
    goto nsq;
qsq:
    if (mem[31])
        goto qsf;
    else
        goto qru;
qrv:
    mem[31] = 0;
    goto qsr;
qsg:
    mem[31] = 1;
    goto qsr;
nsr:
    if (mem[31])
        goto nsg;
    else
        goto nrv;
qrw:
    goto qso;
nso:
    goto nrw;
qmh:
    goto qmk;
nmk:
    goto qoa;
noa:
    goto nmh;
qnr:
    mem[32] = 0;
    goto qrp;
qri:
    mem[32] = 1;
    goto qrp;
nrp:
    if (mem[32])
        goto nri;
    else
        goto nnr;
nns:
    mem[32] = 0;
    goto nrq;
nrj:
    mem[32] = 1;
    goto nrq;
qrq:
    if (mem[32])
        goto qrj;
    else
        goto qns;
qnt:
    mem[32] = 0;
    goto qrr;
qrk:
    mem[32] = 1;
    goto qrr;
nrr:
    if (mem[32])
        goto nrk;
    else
        goto nnt;
qnu:
    goto qrs;
nrs:
    goto nnu;
qnv:
    mem[32] = 0;
    goto qrt;
qrl:
    mem[32] = 1;
    goto qrt;
nrt:
    if (mem[32])
        goto nrl;
    else
        goto nnv;
nnw:
    mem[32] = 0;
    goto nru;
nrm:
    mem[32] = 1;
    goto nru;
qru:
    if (mem[32])
        goto qrm;
    else
        goto qnw;
qnx:
    mem[32] = 0;
    goto qrv;
qrn:
    mem[32] = 1;
    goto qrv;
nrv:
    if (mem[32])
        goto nrn;
    else
        goto nnx;
qro:
    goto qrw;
nrw:
    goto nro;
qmk:
    goto qmp;
nmp:
    goto qng;
nng:
    goto nmk;
qmy:
    goto qnr;
nnr:
    goto nmy;
qns:
    goto qmz;
nmz:
    goto nns;
qna:
    goto qnt;
nnt:
    goto nna;
qnb:
    goto qnu;
nnu:
    goto nnb;
qno:
    goto qnv;
nnv:
    goto nno;
qnw:
    goto qnp;
nnp:
    goto nnw;
qnq:
    goto qnx;
nnx:
    goto nnq;
nmq:
// dead-end
qmp:
    goto qmw;
nmw:
    mem[34] = acc;
    goto qms;
qmr:
    acc = mem[34];
    goto nmr;
nms:
    goto nmp;
qmt:
    goto qmy;
nmy:
    goto nmt;
qmz:
    goto qmu;
nmu:
    goto nmz;
qmv:
    goto qna;
nna:
    goto nmv;
qmx:
    goto qnb;
nnb:
    goto nmx;
qmq:
    goto qmt;
nmt:
    goto nmq;
qmu:
    goto qmr;
nmr:
    goto nmu;
qms:
    goto qmv;
nmv:
    goto nms;
qmw:
    goto qmx;
nmx:
    goto nmw;
nnh:
// dead-end
qng:
    goto qnn;
nnn:
    mem[36] = acc;
    goto qnj;
qni:
    acc = mem[36];
    goto nni;
nnj:
    goto nng;
qnk:
    goto qno;
nno:
    goto nnk;
qnp:
    goto qnl;
nnl:
    goto nnp;
qnm:
    goto qnq;
nnq:
    goto nnm;
qnh:
    goto qnk;
nnk:
    goto nnh;
qnl:
    goto qni;
nni:
    goto nnl;
qnj:
    goto qnm;
nnm:
    goto nnj;
qnn:
    acc = 1;
    goto nnn;
qoa:
    goto qoe;
noe:
    if (acc)
        goto qon;
    else
        goto noa;
non:
    goto qoe;
qoi:
    mem[37] = 0;
    goto qri;
qrb:
    mem[37] = 1;
    goto qri;
nri:
    if (mem[37])
        goto nrb;
    else
        goto noi;
noj:
    mem[37] = 0;
    goto nrj;
nrc:
    mem[37] = 1;
    goto nrj;
qrj:
    if (mem[37])
        goto qrc;
    else
        goto qoj;
qok:
    mem[37] = 0;
    goto qrk;
qrd:
    mem[37] = 1;
    goto qrk;
nrk:
    if (mem[37])
        goto nrd;
    else
        goto nok;
qre:
    goto qrl;
nrl:
    goto nre;
qrm:
    goto qrf;
nrf:
    goto nrm;
qrg:
    goto qrn;
nrn:
    goto nrg;
qrh:
    goto qro;
nro:
    goto nrh;
noh:
qog:
// dead-end
qoe:
    goto qof;
nof:
    goto noe;
qof:
    goto qoi;
noi:
    goto nof;
qoj:
    goto qog;
nog:
    goto noj;
qoh:
    goto qok;
nok:
    goto noh;
qon:
    goto qos;
nos:
    goto qqa;
nqa:
    goto non;
qqy:
    goto qrb;
nrb:
    goto nqy;
qrc:
    goto qqz;
nqz:
    goto nrc;
qra:
    goto qrd;
nrd:
    goto nra;
qps:
    goto qre;
nre:
    goto nps;
qrf:
    goto qpt;
npt:
    goto nrf;
qpu:
    goto qrg;
nrg:
    goto npu;
qpv:
    goto qrh;
nrh:
    goto npv;
not:
// dead-end
qos:
    goto qpb;
npb:
    mem[39] = acc;
    goto qov;
qou:
    acc = mem[39];
    goto nou;
nov:
    goto nos;
qow:
    mem[40] = 0;
    goto qps;
qpo:
    mem[40] = 1;
    goto qps;
nps:
    if (mem[40])
        goto npo;
    else
        goto now;
nox:
    mem[40] = 0;
    goto npt;
npp:
    mem[40] = 1;
    goto npt;
qpt:
    if (mem[40])
        goto qpp;
    else
        goto qox;
qoy:
    mem[40] = 0;
    goto qpu;
qpq:
    mem[40] = 1;
    goto qpu;
npu:
    if (mem[40])
        goto npq;
    else
        goto noy;
qpr:
    goto qpv;
npv:
    goto npr;
qot:
    goto qow;
now:
    goto not;
qox:
    goto qou;
nou:
    goto nox;
qov:
    goto qoy;
noy:
    goto nov;
qpb:
    goto qpf;
npf:
    mem[41] = acc;
    goto qpm;
npm:
    acc = mem[41] * acc;
    goto npb;
qpj:
    goto qpo;
npo:
    goto npj;
qpp:
    goto qpk;
npk:
    goto npp;
qpl:
    goto qpq;
npq:
    goto npl;
qpn:
    goto qpr;
npr:
    goto npn;
npi:
qph:
// dead-end
qpf:
    goto qpg;
npg:
    goto npf;
qpg:
    goto qpj;
npj:
    goto npg;
qpk:
    goto qph;
nph:
    goto npk;
qpi:
    goto qpl;
npl:
    goto npi;
qpm:
    goto qpn;
npn:
    goto npm;
nqb:
// dead-end
qqa:
    goto qqj;
nqj:
    mem[43] = acc;
    goto qqd;
qqc:
    acc = mem[43];
    goto nqc;
nqd:
    goto nqa;
qqe:
    mem[44] = 0;
    goto qqy;
qqv:
    mem[44] = 1;
    goto qqy;
nqy:
    if (mem[44])
        goto nqv;
    else
        goto nqe;
nqf:
    mem[44] = 0;
    goto nqz;
nqw:
    mem[44] = 1;
    goto nqz;
qqz:
    if (mem[44])
        goto qqw;
    else
        goto qqf;
qqg:
    mem[44] = 0;
    goto qra;
qqx:
    mem[44] = 1;
    goto qra;
nra:
    if (mem[44])
        goto nqx;
    else
        goto nqg;
qqb:
    goto qqe;
nqe:
    goto nqb;
qqf:
    goto qqc;
nqc:
    goto nqf;
qqd:
    goto qqg;
nqg:
    goto nqd;
qqj:
    goto qqn;
nqn:
    mem[45] = acc;
    goto qqu;
nqu:
    acc = mem[45] - acc;
    goto nqj;
qqr:
    goto qqv;
nqv:
    goto nqr;
qqw:
    goto qqs;
nqs:
    goto nqw;
qqt:
    goto qqx;
nqx:
    goto nqt;
nqq:
qqp:
// dead-end
qqn:
    goto qqo;
nqo:
    goto nqn;
qqo:
    goto qqr;
nqr:
    goto nqo;
qqs:
    goto qqp;
nqp:
    goto nqs;
qqq:
    goto qqt;
nqt:
    goto nqq;
qqu:
    acc = 1;
    goto nqu;
nsd:
qsc:
// dead-end
qsa:
    goto qsb;
nsb:
    goto nsa;
qsb:
    goto qse;
nse:
    goto nsb;
qsf:
    goto qsc;
nsc:
    goto nsf;
qsd:
    goto qsg;
nsg:
    goto nsd;
qsp:
    acc = mem[46];
    goto nsp;
qsr:
    goto qsq;
nsq:
    mem[46] = acc;
    goto nsr;
qss:
    acc = mem[47];
    goto nss;
qsu:
    goto qst;
nst:
    mem[47] = acc;
    goto nsu;

nc:
    printf("%d\n", acc);
}
