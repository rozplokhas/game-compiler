#include <stdio.h>
 
int main(void) {
    int acc = 0;
    static int mem[19];
    goto qc;
 
qc:
     goto qf;
nf:
     goto qey;
ney:
     goto nc;
qen:
    goto qfq;
qfr:
    goto qeo;
qep:
    goto qfs;
qeq:
    goto qft;
qer:
    mem[0] = 0;
    goto qfn;
qfc:
    mem[0] = 1;
    goto qfn;
qfo:
    if (mem[0])
        goto qfd;
    else
        goto qes;
qet:
    mem[0] = 0;
    goto qfp;
qfe:
    mem[0] = 1;
    goto qfp;
qeu:
    goto qfu;
nfq:
    goto nen;
neo:
    goto nfr;
nfs:
    goto nep;
nft:
    goto neq;
nfn:
    if (mem[0])
        goto nfc;
    else
        goto ner;
nes:
    mem[0] = 0;
    goto nfo;
nfd:
    mem[0] = 1;
    goto nfo;
nfp:
    if (mem[0])
        goto nfe;
    else
        goto net;
nfu:
    goto neu;
qf:
     goto qi;
ni:
     goto qay;
nay:
     goto nf;
qap:
    mem[1] = 0;
    goto qen;
qeg:
    mem[1] = 1;
    goto qen;
qeo:
    if (mem[1])
        goto qeh;
    else
        goto qaq;
qar:
    mem[1] = 0;
    goto qep;
qei:
    mem[1] = 1;
    goto qep;
qas:
    goto qeq;
qat:
    mem[1] = 0;
    goto qer;
qej:
    mem[1] = 1;
    goto qer;
qes:
    if (mem[1])
        goto qek;
    else
        goto qau;
qav:
    mem[1] = 0;
    goto qet;
qel:
    mem[1] = 1;
    goto qet;
qem:
    goto qeu;
nen:
    if (mem[1])
        goto neg;
    else
        goto nap;
naq:
    mem[1] = 0;
    goto neo;
neh:
    mem[1] = 1;
    goto neo;
nep:
    if (mem[1])
        goto nei;
    else
        goto nar;
neq:
    goto nas;
ner:
    if (mem[1])
        goto nej;
    else
        goto nat;
nau:
    mem[1] = 0;
    goto nes;
nek:
    mem[1] = 1;
    goto nes;
net:
    if (mem[1])
        goto nel;
    else
        goto nav;
neu:
    goto nem;
qi:
     goto qn;
nn:
     goto qae;
nae:
     goto ni;
qw:
    goto qap;
qaq:
    goto qx;
qy:
    goto qar;
qz:
    goto qas;
qam:
    goto qat;
qau:
    goto qan;
qao:
    goto qav;
nap:
    goto nw;
nx:
    goto naq;
nar:
    goto ny;
nas:
    goto nz;
nat:
    goto nam;
nan:
    goto nau;
nav:
    goto nao;
no:
// dead-end
qn:
    goto qu;
nu:
    mem[3] = acc;
    goto qq;
qp:
    acc = mem[3];
    goto np;
nq:
    goto nn;
qr:
    goto qw;
qx:
    goto qs;
qt:
    goto qy;
qv:
    goto qz;
nw:
    goto nr;
ns:
    goto nx;
ny:
    goto nt;
nz:
    goto nv;
qo:
    goto qr;
nr:
    goto no;
qs:
    goto qp;
np:
    goto ns;
qq:
    goto qt;
nt:
    goto nq;
qu:
    goto qv;
nv:
    goto nu;
naf:
// dead-end
qae:
    goto qal;
nal:
    mem[5] = acc;
    goto qah;
qag:
    acc = mem[5];
    goto nag;
nah:
    goto nae;
qai:
    goto qam;
qan:
    goto qaj;
qak:
    goto qao;
nam:
    goto nai;
naj:
    goto nan;
nao:
    goto nak;
qaf:
    goto qai;
nai:
    goto naf;
qaj:
    goto qag;
nag:
    goto naj;
qah:
    goto qak;
nak:
    goto nah;
qal:
    acc = 1;
    goto nal;
qay:
    goto qbc;
nbc:
    if (acc)
        goto qbl;
    else
        goto nay;
nbl:
    goto qbc;
qbg:
    mem[7] = 0;
    goto qeg;
qdz:
    mem[7] = 1;
    goto qeg;
qeh:
    if (mem[7])
        goto qea;
    else
        goto qbh;
qbi:
    mem[7] = 0;
    goto qei;
qeb:
    mem[7] = 1;
    goto qei;
qec:
    goto qej;
qek:
    goto qed;
qee:
    goto qel;
qef:
    goto qem;
neg:
    if (mem[7])
        goto ndz;
    else
        goto nbg;
nbh:
    mem[7] = 0;
    goto neh;
nea:
    mem[7] = 1;
    goto neh;
nei:
    if (mem[7])
        goto neb;
    else
        goto nbi;
nej:
    goto nec;
ned:
    goto nek;
nel:
    goto nee;
nem:
    goto nef;
nbf:
qbe:
// dead-end
qbc:
    goto qbd;
nbd:
    goto nbc;
qbd:
    goto qbg;
nbg:
    goto nbd;
qbh:
    goto qbe;
nbe:
    goto nbh;
qbf:
    goto qbi;
nbi:
    goto nbf;
qbl:
     goto qbq;
nbq:
     goto qcy;
ncy:
     goto nbl;
qdw:
    goto qdz;
qea:
    goto qdx;
qdy:
    goto qeb;
qcq:
    goto qec;
qed:
    goto qcr;
qcs:
    goto qee;
qct:
    goto qef;
ndz:
    goto ndw;
ndx:
    goto nea;
neb:
    goto ndy;
nec:
    goto ncq;
ncr:
    goto ned;
nee:
    goto ncs;
nef:
    goto nct;
nbr:
// dead-end
qbq:
    goto qbz;
nbz:
    mem[9] = acc;
    goto qbt;
qbs:
    acc = mem[9];
    goto nbs;
nbt:
    goto nbq;
qbu:
    mem[10] = 0;
    goto qcq;
qcm:
    mem[10] = 1;
    goto qcq;
qcr:
    if (mem[10])
        goto qcn;
    else
        goto qbv;
qbw:
    mem[10] = 0;
    goto qcs;
qco:
    mem[10] = 1;
    goto qcs;
qcp:
    goto qct;
ncq:
    if (mem[10])
        goto ncm;
    else
        goto nbu;
nbv:
    mem[10] = 0;
    goto ncr;
ncn:
    mem[10] = 1;
    goto ncr;
ncs:
    if (mem[10])
        goto nco;
    else
        goto nbw;
nct:
    goto ncp;
qbr:
    goto qbu;
nbu:
    goto nbr;
qbv:
    goto qbs;
nbs:
    goto nbv;
qbt:
    goto qbw;
nbw:
    goto nbt;
qbz:
    goto qcd;
ncd:
    mem[11] = acc;
    goto qck;
nck:
    acc = mem[11] * acc;
    goto nbz;
qch:
    goto qcm;
qcn:
    goto qci;
qcj:
    goto qco;
qcl:
    goto qcp;
ncm:
    goto nch;
nci:
    goto ncn;
nco:
    goto ncj;
ncp:
    goto ncl;
ncg:
qcf:
// dead-end
qcd:
    goto qce;
nce:
    goto ncd;
qce:
    goto qch;
nch:
    goto nce;
qci:
    goto qcf;
ncf:
    goto nci;
qcg:
    goto qcj;
ncj:
    goto ncg;
qck:
    goto qcl;
ncl:
    goto nck;
ncz:
// dead-end
qcy:
    goto qdh;
ndh:
    mem[13] = acc;
    goto qdb;
qda:
    acc = mem[13];
    goto nda;
ndb:
    goto ncy;
qdc:
    mem[14] = 0;
    goto qdw;
qdt:
    mem[14] = 1;
    goto qdw;
qdx:
    if (mem[14])
        goto qdu;
    else
        goto qdd;
qde:
    mem[14] = 0;
    goto qdy;
qdv:
    mem[14] = 1;
    goto qdy;
ndw:
    if (mem[14])
        goto ndt;
    else
        goto ndc;
ndd:
    mem[14] = 0;
    goto ndx;
ndu:
    mem[14] = 1;
    goto ndx;
ndy:
    if (mem[14])
        goto ndv;
    else
        goto nde;
qcz:
    goto qdc;
ndc:
    goto ncz;
qdd:
    goto qda;
nda:
    goto ndd;
qdb:
    goto qde;
nde:
    goto ndb;
qdh:
    goto qdl;
ndl:
    mem[15] = acc;
    goto qds;
nds:
    acc = mem[15] - acc;
    goto ndh;
qdp:
    goto qdt;
qdu:
    goto qdq;
qdr:
    goto qdv;
ndt:
    goto ndp;
ndq:
    goto ndu;
ndv:
    goto ndr;
ndo:
qdn:
// dead-end
qdl:
    goto qdm;
ndm:
    goto ndl;
qdm:
    goto qdp;
ndp:
    goto ndm;
qdq:
    goto qdn;
ndn:
    goto ndq;
qdo:
    goto qdr;
ndr:
    goto ndo;
qds:
    acc = 1;
    goto nds;
nfb:
qfa:
// dead-end
qey:
    goto qez;
nez:
    goto ney;
qez:
    goto qfc;
nfc:
    goto nez;
qfd:
    goto qfa;
nfa:
    goto nfd;
qfb:
    goto qfe;
nfe:
    goto nfb;
qfn:
    acc = mem[17];
    goto nfn;
qfp:
    goto qfo;
nfo:
    mem[17] = acc;
    goto nfp;
qfq:
    acc = mem[18];
    goto nfq;
qfs:
    goto qfr;
nfr:
    mem[18] = acc;
    goto nfs;
qft:
    acc = 10;
    goto nft;
qfu:
    acc = 2;
    goto nfu;

nc:
    printf("%d\n", acc);
}
