n=in;
L1:
    out(n);
    unless(n) goto L2;
    n=n-1;
    goto L1;
L2:
    halt;
    int n;