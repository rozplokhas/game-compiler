#include <stdio.h>

int main(void) {
    int acc = 0;
    static int mem[41];
    goto qj;

nab:
    mem[1] = 1;
    goto nci;
nai:
    mem[3] = acc;
    goto qax;
nal:
    mem[4] = acc;
    goto qaw;
nao:
    mem[5] = acc;
    goto qav;
nap:
    mem[6] = acc;
    goto qat;
nas:
    acc = mem[6] - acc;
    goto nao;
nat:
    mem[7] = acc;
    goto qau;
nau:
    acc = mem[7] * acc;
    goto nas;
nav:
    acc = mem[5] - acc;
    goto nal;
naw:
    acc = mem[4] - acc;
    goto nai;
nax:
    acc = mem[3] == acc;
    goto nab;
naz:
    if (mem[1])
        goto qdx;
    else
        goto qac;
nba:
    mem[0] = 0;
    goto nrl;
nbb:
    mem[0] = 0;
    goto nov;
nbq:
    mem[0] = 1;
    goto nrl;
nbr:
    mem[0] = 1;
    goto nov;
nci:
    if (acc)
        goto qck;
    else
        goto qct;
nct:
    if (acc)
        goto qcu;
    else
        goto naz;
ndx:
    mem[10] = acc;
    goto qfn;
nem:
    mem[11] = acc;
    goto qfq;
nfh:
    mem[12] = acc;
    goto qft;
nfm:
    mem[9] = 0;
    goto nmp;
nfp:
    mem[9] = 0;
    goto nnw;
nfs:
    mem[9] = 0;
    goto nnt;
nfz:
    if (acc)
        goto qik;
    else
        goto qnf;
ngd:
    mem[14] = acc;
    goto qbs;
ngk:
    acc = mem[14] != acc;
    goto nfz;
ngn:
    mem[13] = 0;
    goto nml;
nhf:
    mem[17] = 0;
    goto nio;
nhj:
    mem[16] = acc;
    goto qhg;
nhn:
    mem[18] = acc;
    goto qih;
nhx:
    acc = mem[18] + acc;
    goto nhj;
ni:
    mem[1] = 0;
    goto nci;
nii:
    mem[17] = 1;
    goto nio;
nil:
    mem[15] = 0;
    goto nmf;
nio:
    mem[15] = 0;
    goto nmi;
niz:
    mem[20] = 0;
    goto nkf;
njd:
    mem[19] = acc;
    goto qja;
njh:
    mem[21] = acc;
    goto qjy;
njr:
    acc = mem[21] - acc;
    goto njd;
njz:
    mem[20] = 1;
    goto nkf;
nkf:
    mem[15] = 1;
    goto nmf;
nki:
    mem[15] = 1;
    goto nmi;
nkk:
    if (mem[15])
        goto njr;
    else
        goto nhn;
nkn:
    if (mem[15])
        goto njh;
    else
        goto nhx;
nkz:
    mem[23] = 0;
    goto nmc;
nld:
    mem[22] = acc;
    goto qla;
nlh:
    mem[24] = acc;
    goto qlo;
nlo:
    acc = mem[24] + acc;
    goto nld;
nlq:
    mem[23] = 1;
    goto nmc;
nmc:
    mem[13] = 1;
    goto nml;
nmf:
    mem[9] = 1;
    goto nmp;
nmi:
    mem[9] = 1;
    goto nnw;
nmk:
    if (mem[13])
        goto nlh;
    else
        goto ngd;
nml:
    mem[9] = 1;
    goto nnt;
nmp:
    mem[8] = 0;
    goto nnz;
nmq:
    if (mem[9])
        goto qlp;
    else
        goto qem;
nmt:
    if (mem[9])
        goto qkh;
    else
        goto qfh;
nmw:
    if (mem[9])
        goto qgm;
    else
        goto qgm;
nng:
    mem[8] = 1;
    goto nnz;
nnt:
    mem[25] = acc;
    goto nmw;
nnw:
    mem[26] = acc;
    goto nmt;
nny:
    if (mem[8])
        goto nc;
    else
        goto nkk;
nnz:
    mem[27] = acc;
    goto nmq;
nod:
    if (mem[0])
        goto ngk;
    else
        goto np;
nov:
    mem[30] = acc;
    goto qps;
np:
    mem[2] = acc;
    goto qv;
npm:
    mem[31] = acc;
    goto qpw;
npr:
    mem[29] = 0;
    goto nus;
npv:
    mem[29] = 0;
    goto ntt;
nqd:
    if (acc)
        goto qrn;
    else
        goto qud;
nqi:
    mem[32] = 0;
    goto nti;
nqw:
    mem[34] = 0;
    goto ntl;
nra:
    mem[33] = acc;
    goto qqx;
nre:
    mem[35] = acc;
    goto qun;
nrl:
    acc = mem[35] * acc;
    goto nra;
nro:
    mem[34] = 1;
    goto ntl;
nse:
    mem[37] = 0;
    goto ntb;
nsi:
    mem[36] = acc;
    goto qsf;
nsm:
    mem[38] = acc;
    goto qst;
nst:
    acc = mem[38] - acc;
    goto nsi;
nsv:
    mem[37] = 1;
    goto ntb;
ntb:
    mem[32] = 1;
    goto nti;
nth:
    if (mem[32])
        goto nsm;
    else
        goto nqd;
nti:
    mem[29] = 1;
    goto nus;
ntl:
    mem[29] = 1;
    goto ntt;
ntq:
    if (mem[29])
        goto qqh;
    else
        goto qpm;
ntt:
    mem[28] = 0;
    goto nup;
ntu:
    if (mem[29])
        goto qsu;
    else
        goto qqh;
nue:
    mem[28] = 1;
    goto nup;
nuo:
    if (mem[28])
        goto nod;
    else
        goto nre;
nup:
    mem[39] = acc;
    goto ntu;
nus:
    mem[40] = acc;
    goto ntq;
nv:
    acc = mem[2] == acc;
    goto ni;
qac:
    mem[1] = 1;
    goto qay;
qap:
    acc = 4;
    goto nap;
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
qay:
    if (mem[1])
        goto qap;
    else
        goto qbc;
qbc:
    mem[0] = 0;
    goto quj;
qbs:
    mem[0] = 1;
    goto quj;
qbt:
    acc = 4;
    goto nbq;
qck:
    acc = 0;
    goto naz;
qct:
    acc = 1;
    goto nct;
qcu:
    acc = 0;
    goto qct;
qcz:
    acc = 2;
    goto nbr;
qds:
    acc = mem[10];
    goto nfm;
qdx:
    acc = 0;
    goto ndx;
qeh:
    acc = mem[11];
    goto nfp;
qem:
    acc = 1;
    goto nem;
qfc:
    acc = mem[12];
    goto nfs;
qfh:
    acc = 0;
    goto nfh;
qfl:
    mem[9] = 0;
    goto qmo;
qfn:
    mem[9] = 0;
    goto qmq;
qfo:
    mem[9] = 0;
    goto qnv;
qfq:
    mem[9] = 0;
    goto qms;
qfr:
    mem[9] = 0;
    goto qns;
qft:
    mem[9] = 0;
    goto qmv;
qgm:
    mem[13] = 0;
    goto qmk;
qgo:
    mem[13] = 0;
    goto qmm;
qhc:
    acc = mem[16];
    goto nhf;
qhe:
    mem[17] = 0;
    goto qin;
qhg:
    mem[17] = 0;
    goto qip;
qih:
    mem[17] = 1;
    goto qin;
qij:
    mem[17] = 1;
    goto qip;
qik:
    mem[15] = 0;
    goto qme;
qim:
    mem[15] = 0;
    goto qmg;
qin:
    mem[15] = 0;
    goto qmh;
qip:
    mem[15] = 0;
    goto qmj;
qiw:
    acc = mem[19];
    goto niz;
qiy:
    mem[20] = 0;
    goto qke;
qj:
    mem[1] = 0;
    goto qay;
qja:
    mem[20] = 0;
    goto qkg;
qjy:
    mem[20] = 1;
    goto qke;
qka:
    mem[20] = 1;
    goto qkg;
qke:
    mem[15] = 1;
    goto qme;
qkg:
    mem[15] = 1;
    goto qmg;
qkh:
    mem[15] = 1;
    goto qmh;
qkj:
    mem[15] = 1;
    goto qmj;
qkw:
    acc = mem[22];
    goto nkz;
qky:
    mem[23] = 0;
    goto qmb;
qla:
    mem[23] = 0;
    goto qmd;
qlo:
    acc = 1;
    goto nlo;
qlp:
    mem[23] = 1;
    goto qmb;
qlr:
    mem[23] = 1;
    goto qmd;
qmb:
    mem[13] = 1;
    goto qmk;
qmd:
    mem[13] = 1;
    goto qmm;
qme:
    mem[9] = 1;
    goto qmo;
qmg:
    mem[9] = 1;
    goto qmq;
qmh:
    mem[9] = 1;
    goto qnv;
qmj:
    mem[9] = 1;
    goto qms;
qmk:
    mem[9] = 1;
    goto qns;
qmm:
    mem[9] = 1;
    goto qmv;
qmo:
    mem[8] = 0;
    goto qny;
qmp:
    if (mem[9])
        goto qiw;
    else
        goto qds;
qmq:
    mem[8] = 0;
    goto qmp;
qms:
    if (mem[9])
        goto qhc;
    else
        goto qeh;
qmv:
    if (mem[9])
        goto qkw;
    else
        goto qfc;
qnf:
    mem[8] = 1;
    goto qny;
qnh:
    mem[8] = 1;
    goto qmp;
qns:
    acc = mem[25];
    goto nmk;
qnv:
    acc = mem[26];
    goto nkn;
qny:
    acc = mem[27];
    goto nny;
qoq:
    acc = mem[30];
    goto npr;
qph:
    acc = mem[31];
    goto npv;
qpm:
    acc = 1;
    goto npm;
qpq:
    mem[29] = 0;
    goto qur;
qps:
    mem[29] = 0;
    goto qtp;
qpu:
    mem[29] = 0;
    goto qts;
qpw:
    mem[29] = 0;
    goto qtu;
qqh:
    mem[32] = 0;
    goto qth;
qqj:
    mem[32] = 0;
    goto qtj;
qqt:
    acc = mem[33];
    goto nqw;
qqv:
    mem[34] = 0;
    goto qtk;
qqx:
    mem[34] = 0;
    goto qtm;
qrn:
    mem[34] = 1;
    goto qtk;
qrp:
    mem[34] = 1;
    goto qtm;
qsb:
    acc = mem[36];
    goto nse;
qsd:
    mem[37] = 0;
    goto qta;
qsf:
    mem[37] = 0;
    goto qtc;
qst:
    acc = 1;
    goto nst;
qsu:
    mem[37] = 1;
    goto qta;
qsw:
    mem[37] = 1;
    goto qtc;
qt:
    acc = 2;
    goto nba;
qta:
    mem[32] = 1;
    goto qth;
qtc:
    mem[32] = 1;
    goto qtj;
qth:
    mem[29] = 1;
    goto qur;
qtj:
    mem[29] = 1;
    goto qtp;
qtk:
    mem[29] = 1;
    goto qts;
qtm:
    mem[29] = 1;
    goto qtu;
qtp:
    if (mem[29])
        goto qsb;
    else
        goto qoq;
qts:
    mem[28] = 0;
    goto quo;
qtt:
    if (mem[29])
        goto qqt;
    else
        goto qph;
qtu:
    mem[28] = 0;
    goto qtt;
qu:
    acc = 10;
    goto nbb;
qud:
    mem[28] = 1;
    goto quo;
quf:
    mem[28] = 1;
    goto qtt;
quj:
    if (mem[0])
        goto qcz;
    else
        goto qu;
qun:
    if (mem[0])
        goto qbt;
    else
        goto qt;
quo:
    acc = mem[39];
    goto nuo;
qur:
    acc = mem[40];
    goto nth;
qv:
    acc = 1024;
    goto nv;

nc:
    printf("%d\n", acc);
}
