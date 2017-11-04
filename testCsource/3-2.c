L1: if(c==4) goto L2;
        unless(c==0) goto else1;
        DivA=in;
        DivB=1000*10;
        goto fi;
else1:  unless(c==1) goto else2;
        DivA=Mod;
        DivB=1000;
        goto fi;
else2:  unless(c==2) goto else3;
        DivA=Mod;
        DivB=100;
        goto fi;
else3:  DivA=Mod;
        DivB=10;
fi:
    goto Div;
returnDiv:
    printNum=printNum*16+Quo;
    c=c+1;
    goto L1;
L2:
    printNum=printNum*16+Mod;
    out(printNum);
    halt;


int c=0;
int printNum=0;


Div:
    Quo=0;
    divtmp1=DivA;
inDiv1:    if(DivA<DivB) goto inDiv2;
    DivA=DivA-DivB;
    Quo=Quo+1;
    goto inDiv1;
inDiv2:
    Mod=divtmp1-Quo*DivB;
    goto returnDiv;
int divtmp1;
int DivA;
int DivB;
int Quo;
int Mod;
