
Program  PL0 ;
{带有代码生成的PL0编译程序}

Const 
  norw = 13; {保留字的个数}
  txmax = 100; {标识符表长度}
  nmax = 14; {数字的最大位数}
  al = 10; {标识符的长度}
  amax = 2047; {最大地址}
  levmax = 3; {程序体嵌套的最大深度}
  cxmax = 200; {代码数组的大小}

Type 
  symbol = (nul, ident, number, plus, minus, times, slash, oddsym,
            eql, neq, lss, leq, gtr, geq, lparen, rparen, comma, semicolon,
            period, becomes, beginsym, endsym, ifsym, thensym,
            whilesym, dosym, callsym, constsym, varsym, procsym, redsym, wrtsym);
  alfa = packed array [1..al] Of char;
  m_object = (constant, variable, m_procedure);
  symset = set Of symbol;
  fct = (lit, opr, lod, sto, cal, int, jmp, jpc, red, wrt); {functions}
  instruction = packed Record
    f : fct;  {功能码}
    l : 0..levmax; {相对层数}
    a : 0..amax; {相对地址}
  End;



{LIT 0,a : 取常数a
  OPR 0,a : 执行运算a
  LOD l,a : 取层差为l的层﹑相对地址为a的变量
  STO l,a : 存到层差为l的层﹑相对地址为a的变量
  CAL l,a : 调用层差为l的过程
  INT 0,a : t寄存器增加a
  JMP 0,a : 转移到指令地址a处
  JPC 0,a : 条件转移到指令地址a处 }

Var 
  ch : char; {最近读到的字符}
  sym : symbol; {最近读到的符号}
  id : alfa; {最近读到的标识符}
  num : integer; {最近读到的数}
  cc : integer; {当前行的字符计数}
  ll : integer; {当前行的长度}
  kk, err : integer;
  cx : integer; {代码数组的当前下标}
  line : array [1..81] Of char;
  a : alfa;
  code : array [0..cxmax] Of instruction;
  word : array [1..norw] Of alfa;
  wsym : array [1..norw] Of symbol;
  ssym : array [char] Of symbol;
  mnemonic : array [fct] Of packed array [1..5] Of char;
  declbegsys, statbegsys, facbegsys : symset;
  table : array [0..txmax] Of 
          Record
            name : alfa;
            Case kind : m_object Of 
              constant : (val : integer);
              variable, m_procedure : (level, adr : integer)
          End;
  sfin, sfout, ifout, rfout: text; {Source program file & output data file}
  sfinp, sfoutp, ifoutp, rfoutp: string; {Source program file name}
Procedure error (n : integer);
Begin
  writeln(sfout, '****', ' ' : cc-1, '↑', n : 2);
  err := err + 1
End {error};
Procedure getsym;

Var  i, j, k : integer;
Procedure  getch ;
Begin
  If cc = ll Then
    Begin
      If eof(sfin) Then
        Begin
          write('PROGRAM INCOMPLETE');
          close(sfin);
          close(sfout);
          close(ifout);
          close(rfout);
          exit;
        End;
      ll := 0;
      cc := 0;
      write(sfout, cx : 5, ' ');
      While Not eoln(sfin) Do
        Begin
          ll := ll + 1;
          read(sfin, ch);
          write(sfout, ch);
          line[ll] := ch
        End;
      writeln(sfout);
      readln(sfin);
      ll := ll + 1;
      line[ll] := ' ';
    End;
  cc := cc + 1;
  ch := line[cc]
End {getch};
Begin {getsym}
  While ch = ' ' Do
    getch;
  If ch In ['a'..'z'] Then
    Begin {标识符或保留字}
      k := 0;
      Repeat
        If k < al Then
          Begin
            k := k + 1;
            a[k] := ch
          End;
        getch
      Until Not (ch In ['a'..'z', '0'..'9']);
      If k >= kk  Then kk := k
      Else
        Repeat
          a[kk] := ' ';
          kk := kk-1
        Until kk = k;
      id := a;
      i := 1;
      j := norw;
      Repeat
        k := (i+j) Div 2;
        If id <= word[k] Then j := k-1;
        If id >= word[k] Then i := k + 1
      Until i > j;
      If i-1 > j Then sym := wsym[k]
      Else sym := ident
    End
  Else
    If ch In ['0'..'9'] Then
      Begin {数字}
        k := 0;
        num := 0;
        sym := number;
        Repeat
          num := 10*num + (ord(ch)-ord('0'));
          k := k + 1;
          getch;
        Until Not (ch In ['0'..'9']);
        If k > nmax Then error(30)
      End
  Else
    If ch = ':' Then
      Begin
        getch;
        If ch = '=' Then
          Begin
            sym := becomes;
            getch
          End
        Else  sym := nul;
      End
  Else
    Begin
      sym := ssym[ch];
      getch
    End
End {getsym};
Procedure  gen(x : fct; y, z : integer);
Begin
  If cx > cxmax Then
    Begin
      write('PROGRAM TOO LONG');
      close(sfin);
      close(sfout);
      close(ifout);
      close(rfout);
      exit;
    End;
  With code[cx] Do
    Begin
      f := x;
      l := y;
      a := z
    End;
  cx := cx + 1
End {gen};
Procedure  test(s1, s2 : symset; n : integer);
Begin
  If Not (sym In s1) Then
    Begin
      error(n);
      s1 := s1 + s2;
      While Not (sym In s1) Do
        getsym
    End
End {test};
Procedure  block(lev, tx : integer; fsys : symset);

Var 
  dx : integer; {本过程数据空间分配下标}
  tx0 : integer; {本过程标识表起始下标}
  cx0 : integer; {本过程代码起始下标}
Procedure  enter(k : m_object);
Begin {把m_object填入符号表中}
  tx := tx +1;
  With table[tx] Do
    Begin
      name := id;
      kind := k;
      Case k Of 
        constant :
                   Begin
                     If num > amax Then
                       Begin
                         error(30);
                         num := 0
                       End;
                     val := num
                   End;
        variable :
                   Begin
                     level := lev;
                     adr := dx;
                     dx := dx +1;
                   End;
        m_procedure : level := lev
      End
    End
End {enter};
Function  position(id : alfa) : integer;

Var  i : integer;
Begin {在标识符表中查标识符id}
  table[0].name := id;
  i := tx;
  While table[i].name <> id Do
    i := i-1;
  position := i
End {position};
Procedure constdeclaration;
Begin
  If sym = ident Then
    Begin
      getsym;
      If sym In [eql, becomes] Then
        Begin
          If sym = becomes Then error(1);
          getsym;
          If sym = number Then
            Begin
              enter(constant);
              getsym
            End
          Else error(2)
        End
      Else error(3)
    End
  Else error(4)
End {constdeclaration};
Procedure  vardeclaration;
Begin
  If sym = ident Then
    Begin
      enter(variable);
      getsym
    End
  Else error(4)
End {vardeclaration};
Procedure  listcode;

Var  i : integer;
Begin  {列出本程序体生成的代码}
  For i := cx0 To cx-1 Do
    With code[i] Do
      writeln(ifout, i, mnemonic[f] : 5, l : 3, a : 5)
End {listcode};
Procedure  statement(fsys : symset);

Var  i, cx1, cx2 : integer;
Procedure  expression(fsys : symset);

Var  addop : symbol;
Procedure  term(fsys : symset);

Var  mulop : symbol;
Procedure  factor(fsys : symset);

Var i : integer;
Begin
  test(facbegsys, fsys, 24);
  While sym In facbegsys Do
    Begin
      If sym = ident Then
        Begin
          i := position(id);
          If i = 0 Then error(11)
          Else
            With table[i] Do
              Case kind Of 
                constant : gen(lit, 0, val);
                variable : gen(lod, lev-level, adr);
                m_procedure : error(21)
              End;
          getsym
        End
      Else
        If sym = number Then
          Begin
            If num > amax Then
              Begin
                error(30);
                num := 0
              End;
            gen(lit, 0, num);
            getsym
          End
      Else
        If sym = lparen Then
          Begin
            getsym;
            expression([rparen]+fsys);
            If sym = rparen Then getsym
            Else error(22)
          End;
      test(fsys, [lparen], 23)
    End
End {factor};
Begin {term}
  factor(fsys+[times, slash]);
  While sym In [times, slash] Do
    Begin
      mulop := sym;
      getsym;
      factor(fsys+[times, slash]);
      If mulop = times Then gen(opr, 0, 4)
      Else gen(opr, 0, 5)
    End
End {term};
Begin {expression}
  If sym In [plus, minus] Then
    Begin
      addop := sym;
      getsym;
      term(fsys+[plus, minus]);
      If addop = minus Then gen(opr, 0, 1)
    End
  Else term(fsys+[plus, minus]);
  While sym In [plus, minus] Do
    Begin
      addop := sym;
      getsym;
      term(fsys+[plus, minus]);
      If addop = plus Then gen(opr, 0, 2)
      Else gen(opr, 0, 3)
    End
End {expression};
Procedure  condition(fsys : symset);

Var  relop : symbol;
Begin
  If sym = oddsym Then
    Begin
      getsym;
      expression(fsys);
      gen(opr, 0, 6)
    End
  Else
    Begin
      expression([eql, neq, lss, gtr, leq, geq] + fsys);
      If Not (sym In [eql, neq, lss, leq, gtr, geq]) Then
        error(20)
      Else
        Begin
          relop := sym;
          getsym;
          expression(fsys);
          Case relop Of 
            eql : gen(opr, 0, 8);
            neq : gen(opr, 0, 9);
            lss : gen(opr, 0, 10);
            geq : gen(opr, 0, 11);
            gtr : gen(opr, 0, 12);
            leq : gen(opr, 0, 13);
          End
        End
    End
End {condition};
Begin {statement}
  If sym = ident Then
    Begin
      i := position(id);
      If i = 0 Then error(11)
      Else
        If table[i].kind <> variable Then
          Begin {对非变量赋值}
            error(12);
            i := 0;
          End;
      getsym;
      If sym = becomes Then getsym
      Else error(13);
      expression(fsys);
      If i <> 0 Then
        With table[i] Do
          gen(sto, lev-level, adr)
    End
  Else
    If sym = callsym Then
      Begin
        getsym;
        If sym <> ident Then error(14)
        Else
          Begin
            i := position(id);
            If i = 0 Then error(11)
            Else
              With table[i] Do
                If kind = m_procedure Then
                  gen(cal, lev-level, adr)
                Else error(15);
            getsym
          End
      End
  Else
    If sym = ifsym Then
      Begin
        getsym;
        condition([thensym, dosym]+fsys);
        If sym = thensym Then getsym
        Else error(16);
        cx1 := cx;
        gen(jpc, 0, 0);
        statement(fsys);
        code[cx1].a := cx
      End
  Else
    If sym = beginsym Then
      Begin
        getsym;
        statement([semicolon, endsym]+fsys);
        While sym In [semicolon]+statbegsys Do
          Begin
            If sym = semicolon Then getsym
            Else error(10);
            statement([semicolon, endsym]+fsys)
          End;
        If sym = endsym Then getsym
        Else error(17)
      End
  Else
    If sym = whilesym Then
      Begin
        cx1 := cx;
        getsym;
        condition([dosym]+fsys);
        cx2 := cx;
        gen(jpc, 0, 0);
        If sym = dosym Then getsym
        Else error(18);
        statement(fsys);
        gen(jmp, 0, cx1);
        code[cx2].a := cx
      End
    Else
      If sym = redsym Then
        Begin
          getsym;
          if sym = lparen Then
            Repeat
              getsym;
              if sym = ident then
                begin
                  i := position(id);
                  if i = 0 Then error(11)
                  else if table[i].kind <> variable then
                    begin
                      error(12);
                      i := 0
                    end
                  else with table[i] do gen(red,lev-level,adr)
                end
              else error(4);
              getsym;
            Until sym <> comma
          else error(40);
          if sym <> rparen then error(22);
          getsym
        End
    Else
      If sym = wrtsym Then
        Begin
          getsym;
          if sym = lparen
          then begin
              repeat
                getsym;
                expression([rparen,comma]+fsys);
                gen(wrt,0,0);
              until sym <> comma;
              if sym <> rparen
              then error(22);
              getsym
              end
          else error(40)
        End;
  test(fsys, [ ], 19)
End {statement};
Begin {block}
  dx := 3;
  tx0 := tx;
  table[tx].adr := cx;
  gen(jmp, 0, 0);
  If lev > levmax Then error(32);
  Repeat
    If sym = constsym Then
      Begin
        getsym;
        Repeat
          constdeclaration;
          While sym = comma Do
            Begin
              getsym;
              constdeclaration
            End;
          If sym = semicolon Then getsym
          Else error(5)
        Until sym <> ident
      End;
    If sym = varsym Then
      Begin
        getsym;
        Repeat
          vardeclaration;
          While sym = comma Do
            Begin
              getsym;
              vardeclaration
            End;
          If sym = semicolon Then getsym
          Else error(5)
        Until sym <> ident;
      End;
    While sym = procsym Do
      Begin
        getsym;
        If sym = ident Then
          Begin
            enter(m_procedure);
            getsym
          End
        Else error(4);
        If sym = semicolon Then getsym
        Else error(5);
        block(lev+1, tx, [semicolon]+fsys);
        If sym = semicolon Then
          Begin
            getsym;
            test(statbegsys+[ident, procsym], fsys, 6)
          End
        Else error(5)
      End;
    test(statbegsys+[ident], declbegsys, 7)
  Until Not (sym In declbegsys);
  code[table[tx0].adr].a := cx;
  With table[tx0] Do
    Begin
      adr := cx; {代码开始地址}
    End;
  cx0 := cx;
  gen(int, 0, dx);
  statement([semicolon, endsym]+fsys);
  gen(opr, 0, 0); {生成返回指令}
  test(fsys, [ ], 8);
  listcode;
End  {block};
Procedure  interpret;

Const  stacksize = 500;

Var  p, b, t : integer;
  {程序地址寄存器, 基地址寄存器,栈顶地址寄存器}
  i : instruction; {指令寄存器}
  s : array [1..stacksize] Of integer; {数据存储栈}
Function  base(l : integer) : integer;

Var  b1 : integer;
Begin
  b1 := b; {顺静态链求层差为l的层的基地址}
  While l > 0 Do
    Begin
      b1 := s[b1];
      l := l-1
    End;
  base := b1
End {base};
Begin
  writeln('START PL/0');
  t := 0;
  b := 1;
  p := 0;
  s[1] := 0;
  s[2] := 0;
  s[3] := 0;
  Repeat
    i := code[p];
    p := p+1;
    With i Do
      Case f Of 
        lit :
              Begin
                t := t+1;
                s[t] := a
              End;
        opr : Case a Of {运算}
                0 :
                    Begin {返回}
                      t := b-1;
                      p := s[t+3];
                      b := s[t+2];
                    End;
                1 : s[t] := -s[t];
                2 :
                    Begin
                      t := t-1;
                      s[t] := s[t] + s[t+1]
                    End;
                3 :
                    Begin
                      t := t-1;
                      s[t] := s[t]-s[t+1]
                    End;
                4 :
                    Begin
                      t := t-1;
                      s[t] := s[t] * s[t+1]
                    End;
                5 :
                    Begin
                      t := t-1;
                      s[t] := s[t] Div s[t+1]
                    End;
                6 : s[t] := ord(odd(s[t]));
                8 :
                    Begin
                      t := t-1;
                      s[t] := ord(s[t] = s[t+1])
                    End;
                9:
                   Begin
                     t := t-1;
                     s[t] := ord(s[t] <> s[t+1])
                   End;
                10 :
                     Begin
                       t := t-1;
                       s[t] := ord(s[t] < s[t+1])
                     End;
                11:
                    Begin
                      t := t-1;
                      s[t] := ord(s[t] >= s[t+1])
                    End;
                12 :
                     Begin
                       t := t-1;
                       s[t] := ord(s[t] > s[t+1])
                     End;
                13 :
                     Begin
                       t := t-1;
                       s[t] := ord(s[t] <= s[t+1])
                     End;
              End;
        lod :
              Begin
                t := t + 1;
                s[t] := s[base(l) + a]
              End;
        sto :
              Begin
                s[base(l) + a] := s[t];
                writeln(rfout, s[t]);
                t := t-1
              End;
        cal :
              Begin {generate new block mark}
                s[t+1] := base( l );
                s[t+2] := b;
                s[t+3] := p;
                b := t+1;
                p := a
              End;
        int : t := t + a;
        jmp : p := a;
        jpc :
              Begin
                If s[t] = 0 Then p := a;
                t := t-1;
              End;
        red :
              Begin
                writeln('Running program ask for input: ');
                readln(s[base(l) + a]);
              End;
        wrt :
              Begin
                writeln(s[t]);
                t := t + 1
              End
      End {with, case}
  Until p = 0;
  write('END PL/0');
End {interpret};

Begin  {主程序}
  writeln('Type in the path to your source code: ');
  readln(sfinp);
  writeln('Type in path of the file to output source program at');
  readln(sfoutp);
  writeln('Type in path of the file to output intermediate program at');
  readln(ifoutp);
  writeln('Type in path of the file to output runtime data at');
  readln(rfoutp);
  assign(sfin, sfinp);
  assign(sfout, sfoutp);
  assign(ifout, ifoutp);
  assign(rfout, rfoutp);
  reset(sfin);
  rewrite(sfout);
  rewrite(ifout);
  rewrite(rfout);
  For ch := 'A' To ';' Do
    ssym[ch] := nul;

  word[1] := 'begin     ';
  word[2] := 'call      ';
  word[3] := 'const     ';
  word[4] := 'do        ';
  word[5] := 'end       ';
  word[6] := 'if        ';
  word[7] := 'odd       ';
  word[8] := 'procedure ';
  word[9] := 'read      ';
  word[10] := 'then      ';
  word[11] := 'var       ';
  word[12] := 'while     ';
  word[13] := 'write     ';

  wsym[1] := beginsym;
  wsym[2] := callsym;
  wsym[3] := constsym;
  wsym[4] := dosym;
  wsym[5] := endsym;
  wsym[6] := ifsym;
  wsym[7] := oddsym;
  wsym[8] := procsym;
  wsym[9] := redsym;
  wsym[10] := thensym;
  wsym[11] := varsym;
  wsym[12] := whilesym;
  wsym[13] := wrtsym;

  ssym['+'] := plus;
  ssym['-'] := minus;
  ssym['*'] := times;
  ssym['/'] := slash;
  ssym['('] := lparen;
  ssym[')'] := rparen;
  ssym['='] := eql;
  ssym[','] := comma;
  ssym['.'] := period;
  ssym['&'] := neq;
  ssym['<'] := lss;
  ssym['>'] := gtr;
  ssym[';'] := semicolon;
  ssym['%'] := leq;

  mnemonic[lit] := 'LIT  ';
  mnemonic[opr] := 'OPR  ';
  mnemonic[lod] := 'LOD  ';
  mnemonic[sto] := 'STO  ';
  mnemonic[cal] := 'CAL  ';
  mnemonic[int] := 'INT  ';
  mnemonic[jmp] := 'JMP  ';
  mnemonic[jpc] := 'JPC  ';
  mnemonic[red] := 'RED  ';
  mnemonic[wrt] := 'WRT  ';

  declbegsys := [constsym, varsym, procsym];
  statbegsys := [beginsym, callsym, ifsym, whilesym];
  facbegsys := [ident, number, lparen];
  err := 0;
  cc := 0;
  cx := 0;
  ll := 0;
  ch := ' ';
  kk := al;
  getsym;
  block(0, 0, [period]+declbegsys+statbegsys);
  If sym <> period Then error(9);
  If err = 0 Then interpret
  Else write('ERRORS IN PL/0 PROGRAM');
  writeln;
  close(sfin);
  close(sfout);
  close(ifout);
  close(rfout);
  exit;
End.
