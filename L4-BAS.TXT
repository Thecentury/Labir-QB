10 DECLARE SUB menu ()
DECLARE SUB putsprites ()
DECLARE SUB loadsprites ()
DIM ert AS INTEGER
DIM ocki AS INTEGER
DIM w AS INTEGER, e AS INTEGER
CLEAR , , 15200
time1 = TIMER
ocki = 0
DIM menubil AS INTEGER
DIM minnum AS INTEGER
DIM menudvij AS INTEGER
minnum = 120
DIM kodik2 AS LONG
DIM lihster AS INTEGER
DIM pervtime AS INTEGER
lihster = 1
CONST code1 = 54799326
CONST code2 = 37154921
DIM kodik1 AS LONG
DIM jel AS INTEGER
DIM xist AS INTEGER, yist AS INTEGER
DIM menupokaz AS INTEGER
CONST resdvij = 50
DIM mondvij AS INTEGER
mondvij = 50
CONST vzr = 400
CONST vodadvij = 5
DIM wai AS INTEGER
DIM lednum AS INTEGER
RANDOMIZE TIMER
DIM xexit AS INTEGER, yexit AS INTEGER
DIM skolvomon AS INTEGER
skolvomon = 0
DIM kolvobomb AS INTEGER
kolvobomb = 0
DIM kolvoklu AS INTEGER
kolvoklu = 0
DIM life AS INTEGER
life = 990
DIM kolvomon AS INTEGER
DIM allmon  AS INTEGER
DIM x AS INTEGER, y AS INTEGER
DIM oldx AS INTEGER, oldy AS INTEGER
DIM coordx AS INTEGER, coordy AS INTEGER
DIM n AS INTEGER, v AS INTEGER
DIM zo(200) AS INTEGER
DIM money AS INTEGER
SCREEN 12
WIDTH 80, 60
'LOCATE 10, 5, 0
jel = 1
OPEN "labir.txt" FOR INPUT AS #1
v = 1
DO UNTIL EOF(1)
 v = v + 1
 LINE INPUT #1, l$
LOOP
CLOSE #1
OPEN "labir.txt" FOR INPUT AS #1
DIM a(1 TO v)  AS STRING
FOR u = 1 TO v - 1
 LINE INPUT #1, a(u)
NEXT u
CLOSE #1
n = LEN(a(1))
REDIM b(v, n) AS STRING * 1
FOR q = 1 TO v
 FOR w = 1 TO n
  b(q, w) = MID$(a(q), w, 1)
 NEXT w
NEXT q
DIM z1(200) AS INTEGER 'стена
DIM z2(200) AS INTEGER 'пустота
DIM z3(200) AS INTEGER 'хряпочка
DIM z4(200) AS INTEGER 'монета
DIM z5(200) AS INTEGER 'выход
DIM z6(200) AS INTEGER 'монстр
DIM z7(200) AS INTEGER 'супермонета
DIM jizn(200) AS INTEGER   'жизнь
DIM dver(200) AS INTEGER   'дверь
DIM klut(200) AS INTEGER   'ключ
DIM stena(200) AS INTEGER  'стена
DIM bomb(200) AS INTEGER   'бомба
DIM res1(200) AS INTEGER
DIM res2(200) AS INTEGER
DIM voda1(200) AS INTEGER'вода
DIM voda2(200) AS INTEGER
DIM voda3(200) AS INTEGER
DIM led(200) AS INTEGER 'лед
DIM lodka(200) AS INTEGER'догадайся сам!
DIM smertmon(200) AS INTEGER 'смертьмонстрам
'DIM sar(200) AS INTEGER'шар
DIM krov(200) AS INTEGER'кровища
DIM waternum AS INTEGER
DIM resnum AS INTEGER
'DIM sarnum AS INTEGER
DIM mina(200) AS INTEGER
DIM minst(200) AS INTEGER
DIM per1(500) AS INTEGER
DIM per2(500) AS INTEGER
money = 0
loadsprites
'spravka
'IF menubil = 0 THEN
 menu
'END IF
'IF menubil = 1 AND menudvij = 0 THEN
' menubil = 1
' menudvij = 1
'END IF
putsprites
CIRCLE (629, 469), 10, 11, , , 1
'DIM sarx(sarnum) AS INTEGER
'DIM sary(sarnum) AS INTEGER
'dIM osarx(sarnum) AS INTEGER
'dIM osary(sarnum) AS INTEGER
'DIM sarxv(sarnum) AS INTEGER
'DIM saryv(sarnum) AS INTEGER
'DIM opesar(sarnum) AS INTEGER
'DIM dohsar(sarnum) AS INTEGER
'DIM sav(sarnum) AS INTEGER
DIM xr(resnum) AS INTEGER
DIM yr(resnum) AS INTEGER
DIM ir(resnum) AS INTEGER
DIM rtrig(resnum) AS INTEGER
DIM dohres(resnum) AS INTEGER
waternum1 = waternum
waternum = waternum + lednum
DIM ledx(waternum) AS INTEGER, ledy(waternum) AS INTEGER, hodled(waternum) AS INTEGER
DIM ledtime(waternum)  AS INTEGER
FOR ledind = 1 TO waternum
 ledtime(ledind) = 200
NEXT ledind
DIM xwater(waternum) AS INTEGER
DIM ywater(waternum) AS INTEGER
DIM watertrig(waternum) AS INTEGER
DIM inwat(waternum) AS INTEGER
DIM watlod(waternum) AS INTEGER
FOR yu = 1 TO resnum
 ir(yu) = INT(RND * resdvij)
 tir = RND
 IF tir > .5 THEN
  rtrig(yu) = 0
 ELSE
  rtrig(yu) = 1
 END IF
NEXT yu
dx = 10
dy = 10
x = 310
y = 230
oldx = x
oldy = y
REDIM xo(1 TO v, 1 TO n, 2) AS INTEGER
FOR i = 1 TO v
 FOR u = 1 TO n
 xo(i, u, 1) = 310 - (coordx - u) * 10
 xo(i, u, 2) = 230 - (coordy - i) * 10
 NEXT u
NEXT i
pokmoney = 0
FOR i = 1 TO v
 FOR u = 1 TO n
  IF b(i, u) = "M" THEN kolvomon = kolvomon + 1
 NEXT u
NEXT i
DIM ledneled(waternum) AS INTEGER
DIM watled AS INTEGER
DIM lodkawat AS INTEGER
FOR xwe = 1 TO waternum
 watlod(xwe) = 0
NEXT xwe
FOR i = 1 TO v
 FOR u = 1 TO n
  IF b(i, u) = "w" THEN
   watled = watled + 1
   ledneled(watled) = 0
   xist = 310 - (coordx - u) * 10
   yist = 230 - (coordy - i) * 10
   xwater(watled) = xist
   ywater(watled) = yist
  END IF
  IF b(i, u) = "l" THEN
   watled = watled + 1
   watertrig(watled) = 3
   ledneled(watled) = 1
   xist = 310 - (coordx - u) * 10
   yist = 230 - (coordy - i) * 10
   ledx(watled) = xist
   ledy(watled) = yist
  END IF
 NEXT u
NEXT i
DIM xmon(kolvomon) AS INTEGER
DIM ymon(kolvomon) AS INTEGER
DIM opa(kolvomon) AS INTEGER
DIM estmu(kolvomon) AS INTEGER
moni = 0
rnum = 0
wternum = 0
sardi = 0
DIM bu(kolvomon, 20)  AS INTEGER
FOR yu = 1 TO kolvomon
 bu(yu, 8) = 2
NEXT yu
FOR jbv = 1 TO kolvomon
 opa(jbv) = INT(RND * mondvij)
NEXT jbv
moni = 0
FOR i = 1 TO v
 FOR u = 1 TO n
 IF b(i, u) = "M" THEN
  xist = 310 - (coordx - u) * 10
  yist = 230 - (coordy - i) * 10
  moni = moni + 1
  bu(moni, 6) = i: bu(moni, 7) = u
  xmon(moni) = xist
  ymon(moni) = yist
 END IF
 IF b(i, u) = "r" THEN
  xist = 310 - (coordx - u) * 10
  yist = 230 - (coordy - i) * 10
  rnum = rnum + 1
  xr(rnum) = xist
  yr(rnum) = yist
 END IF
' IF b(i, u) = "h" THEN
'  xist = 310 - (coordx - u) * 10
'  yist = 230 - (coordy - i) * 10
'  sardi = sardi + 1
'  sarx(sardi) = xist
'  sary(sardi) = yist
'  osarx(sardi) = xist
'  osary(sardi) = yist
' END IF
 NEXT u
NEXT i
DIM dxmon(kolvomon) AS INTEGER
DIM dymon(kolvomon) AS INTEGER
REDIM xmon2(kolvomon) AS INTEGER
REDIM ymon2(kolvomon) AS INTEGER
REDIM xold2(kolvomon) AS INTEGER
REDIM yold2(kolvomon) AS INTEGER
DIM dohlmon(kolvomon) AS INTEGER
kolvoklu = 0
bombkol = 0
kolvobomb = 0
alllife = life
billife = life
LOCATE 1, 1
kolvobomb = 999
 FOR i = 1 TO v
  FOR u = 1 TO n
   IF b(i, u) = "e" THEN
    xexit = xo(i, u, 1): yexit = xo(i, u, 2)
   END IF
  NEXT u
 NEXT i

20 DO
cil = 0
LOCATE 51, 76
PRINT life
LOCATE 12, 75
PRINT money
IF jel = 0 THEN
 LOCATE 2, 30
 PRINT "                                   "
 LOCATE 2, 30
 PRINT "Расстояние до выхода:"; CINT(SQR((x - xexit) ^ 2 + (y - yexit) ^ 2) / 10)
END IF
CIRCLE (629, 469), 10, 11, , , 1
IF poprx <> 0 OR popry <> 0 THEN
 FOR i = 1 TO v
  FOR u = 1 TO n
   IF b(i, u) = "e" THEN
    xexit = xo(i, u, 1): yexit = xo(i, u, 2)
   END IF
  NEXT u
 NEXT i
END IF
IF poprx <> 0 OR popry <> 0 THEN
 poprx = 0: popry = 0
END IF
xoukaz = xukaz: youkaz = yukaz
IF SQR((x - xexit) ^ 2 + (y - yexit) ^ 2) <> 0 THEN
 xukaz = 10 * ((y - yexit) / (SQR((x - xexit) ^ 2 + (y - yexit) ^ 2)))
 yukaz = 10 * ((x - xexit) / (SQR((x - xexit) ^ 2 + (y - yexit) ^ 2)))
 LINE (629, 469)-(629 - youkaz, 469 - xoukaz), 0
 LINE (629, 469)-(629 - yukaz, 469 - xukaz), 14
END IF
LOCATE 1, 50
PRINT "Бомб:"; kolvobomb
LOCATE 1, 30
PRINT "Ключей:"; kolvoklu
LOCATE 1, 65
PRINT "Мин:"; minnum
cil = 0
'LOCATE 1, 70
'PRINT "Жизней:"; life
'LOCATE 1, 1
'PRINT "Монет:"; money
ss$ = INKEY$
x2 = x: y2 = y
IF ss$ <> "" THEN
 IF ss$ = CHR$(27) THEN
  menupokaz = 1
  menu
 END IF
 IF nedvij = 0 THEN
  IF MID$(ss$, 2, 1) = "K" THEN 'влево
   x2 = x2 - dx
  ELSEIF MID$(ss$, 2, 1) = "M" THEN 'вправо
   x2 = x2 + dx
  ELSEIF MID$(ss$, 2, 1) = "H" THEN 'вверх
   y2 = y2 - dy
  ELSEIF MID$(ss$, 2, 1) = "P" THEN 'вниз
   y2 = y2 + dy
  END IF
 END IF
 IF ss$ = "[" THEN
  IF pert1 = 1 THEN
   pert1 = 0
   kodik1 = code1 - 232
   PUT (200, 460), per1, PSET
  ELSE
   pert1 = 1
   kodik1 = code1
   PUT (200, 460), per2, PSET
  END IF
 END IF
 IF ss$ = "]" THEN
  IF pert2 = 1 THEN
   pert2 = 0
   kodik2 = code2 - 232
   PUT (215, 460), per1, PSET
  ELSE
   pert2 = 1
   PUT (215, 460), per2, PSET
   kodik2 = code2
  END IF
 END IF
 IF ss$ = "r" THEN 'разминировать
  IF POINT(x + 15, y + 8) = 6 AND POINT(x + 15, y + 6) = 12 THEN 'справа
   zuk = zuk + 1
   LOCATE 1, 1
   PRINT "Идет разминирование..."
   LINE (621, 1)-(638, 99), 13, BF
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x + 10 AND xo(i, u, 2) = y THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   soobs6 = 600
   mx6 = x + 10
   my6 = y
   nedvij = 1
  END IF
  IF POINT(x + 5 - 10, y + 8) = 6 AND POINT(x + 5 - 10, y + 6) = 12 THEN 'слева
   LOCATE 1, 1
   PRINT "Идет разминирование..."
   LINE (621, 1)-(638, 99), 13, BF
   zuk = zuk + 1
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x - 10 AND xo(i, u, 2) = y THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   soobs7 = 600
   mx7 = x - 10
   my7 = y
   nedvij = 1
  END IF
  IF POINT(x + 5, y + 18) = 6 AND POINT(x + 5, y + 16) = 12 THEN 'снизу
   LOCATE 1, 1
   PRINT "Идет разминирование..."
   LINE (621, 1)-(638, 99), 13, BF
   zuk = zuk + 1
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x AND xo(i, u, 2) = y + 10 THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   soobs8 = 600
   mx8 = x
   my8 = y + 10
   nedvij = 1
  END IF
  IF POINT(x + 5, y + 8 - 10) = 6 AND POINT(x + 5, y + 6 - 10) = 12 THEN 'сверху
   LOCATE 1, 1
   PRINT "Идет разминирование..."
   LINE (621, 1)-(638, 99), 13, BF
   zuk = zuk + 1
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x AND xo(i, u, 2) = y - 10 THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   soobs9 = 600
   mx9 = x
   my9 = y - 10
   nedvij = 1
  END IF
  IF zuk = 0 THEN
   LOCATE 1, 1
   PRINT "Мин вокруг вас нет"
   SLEEP 2
   LOCATE 1, 1
   PRINT "                  "
   LINE (0, 0)-(639, 0), 11
  END IF
 ELSEIF ss$ = "m" THEN 'поставить мину
  IF minnum > 0 THEN
'   LOCATE 3, 1
'   PRINT x; "x"; y
   PUT (x, y), minst
   FOR i = 1 TO v
    FOR u = 1 TO n
'     LOCATE 5, 1
'     PRINT xo(i, u, 1); "x"; xo(i, u, 2)
'     SOUND 32000, 1
     IF (x = xo(i, u, 1) - 1 OR x = xo(i, u, 1) + 1 OR x = xo(i, u, 1)) AND (y = xo(i, u, 2) - 1 OR y = xo(i, u, 2) + 1 OR y = xo(i, u, 2)) THEN
'      LOCATE 4, 1
'      PRINT xo(i, u, 1); "x"; xo(i, u, 2)

      b(i, u) = "N"
'      LOCATE 2, 1
'      PRINT "lk"
     END IF
    NEXT u
   NEXT i
   tiktak = 100
   minnum = minnum - 1
   minastoit = 1
  ELSE
   LOCATE 1, 1
   PRINT "У вас нет мин"
   SOUND 2000, 1
   SOUND 1500, 2
   SOUND 1000, 2
   SLEEP 2
   LOCATE 1, 1
   PRINT "             "
  END IF
 ELSEIF ss$ = "b" THEN 'поставить бомбу
  IF kolvobomb > 0 THEN
   IF bombkol = 0 THEN
    PUT (x, y), bomb
    xbomb = x: ybomb = y
    bombkol = 1
   END IF
  ELSE
   LOCATE 1, 14
   PRINT "У вас нет бомб!"
   SOUND 450, 4
   SLEEP 1
   LOCATE 1, 14
   PRINT "               "
   LINE (0, 0)-(639, 0), 11
  END IF
 ELSEIF ss$ = " " THEN 'пауза
  LOCATE 1, 14
  PRINT "ПАУЗА"
  SLEEP
  LOCATE 1, 14
  PRINT "     "
  LINE (0, 0)-(639, 0), 11
 END IF
END IF
'взрыв бомбы
IF bombkol = 1 THEN
 bon = bon + 1
 IF bon = vzr THEN
  bon = 0
  PUT (xbomb, ybomb), bomb, XOR
  FOR radius = 1 TO 4
   CIRCLE (xbomb + 5, ybomb + 5), radius, 12
   SOUND 32000, 2
  NEXT radius
  SOUND 200, 14
  PUT (xbomb, ybomb), z2, PSET
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = xbomb AND xo(i, u, 2) = ybomb THEN
     b(i, u) = "."
    END IF
   NEXT u
  NEXT i
  kolvobomb = kolvobomb - 1
  bombkol = 0
  IF kodik2 = code2 THEN
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF (SQR((xo(i, u, 1) - xbomb) ^ 2 + (xo(i, u, 2) - ybomb) ^ 2) / 10) <= 3 AND b(i, u) = "*" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
     b(i, u) = "."
'     FOR radius = 1 TO 4
'      CIRCLE (xo(i, u, 1) + 5, xo(i, u, 2) + 5), radius, 12
'      SOUND 32000, 1
'     NEXT radius
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
    END IF
   NEXT u
  NEXT i
  END IF
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF (SQR((xo(i, u, 1) - xbomb) ^ 2 + (xo(i, u, 2) - ybomb) ^ 2) / 10) <= 3 AND b(i, u) = "S" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
     b(i, u) = "."
'     FOR radius = 1 TO 4
'      CIRCLE (xo(i, u, 1) + 5, xo(i, u, 2) + 5), radius, 12
'      SOUND 32000, 1
'     NEXT radius
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
    END IF
   NEXT u
  NEXT i
  FOR ui = 1 TO resnum
   IF SQR((xr(ui) - xbomb) ^ 2 + (yr(ui) - ybomb) ^ 2) / 10 <= 3 THEN
    dohres(ui) = 1
    PUT (xr(ui), yr(ui)), z2, PSET
'     FOR radius = 1 TO 4
'      CIRCLE (xr(iu) + 5, yr(iu) + 5), radius, 12
'      SOUND 32000, 1
'     NEXT radius
     PUT (xr(iu), yr(iu)), z2, PSET
    xr(ui) = -10
    yr(ui) = -15
   END IF
  NEXT ui
  FOR jkl = 1 TO kolvomon
   IF SQR((xmon(jkl) - xbomb) ^ 2 + (ymon(jkl) - ybomb) ^ 2) / 10 <= 3 THEN
    dohlmon(jkl) = 1
'    PUT (xmon(jkl), ymon(jkl)), z2, PSET
'     FOR radius = 1 TO 4
'      CIRCLE (xmon(jkl) + 5, ymon(jkl) + 5), radius, 12
'      SOUND 32000, 1
'     NEXT radius
'     PUT (xmon(jkl), ymon(jkl)), z2, PSET
    PUT (xmon(jkl), ymon(jkl)), krov, PSET
    FOR i = 1 TO v
     FOR u = 1 TO n
      IF xmon(jkl) = xo(i, u, 1) AND ymon(jkl) = xo(i, u, 2) THEN b(i, u) = "K"
     NEXT u
    NEXT i
    xmon(jkl) = 0: ymon(jkl) = 0
   END IF
  NEXT jkl
  IF SQR((x - xbomb) ^ 2 + (y - ybomb) ^ 2) / 10 <= 3 THEN
   life = life - 1
   SOUND 1234, 3
   LOCATE 1, 14
   PRINT "Вы контужены взрывом!"
   SLEEP 1
   LOCATE 1, 14
   PRINT "                     "
   LINE (0, 0)-(639, 0), 11
  END IF
  xbomb = 0: ybomb = 0
 END IF
END IF
 k = 0
 k1 = 0
 k2 = 0
 k3 = 0
IF minastoit = 1 THEN
 IF tiktak > 0 THEN
  tiktak = tiktak - 1
  tiki = 1
 ELSEIF tiktak <= 0 AND tiki = 1 THEN
  tiktak = 0
  tiki = 0
 END IF
END IF
IF soobs6 > 0 THEN
 soobs6 = soobs6 - 1
 pos6 = 1
 LOCATE 1, 1
 PRINT "Идет разминирование..."
 nedvij = 1
ELSEIF soobs6 = 0 THEN
 IF pos6 = 1 THEN
  nedvij = 0
  LOCATE 1, 1
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  PUT (mx6, my6), mina, PSET
  pos6 = 0
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = mx6 AND xo(i, u, 2) = my6 THEN
     b(i, u) = "i"
    END IF
   NEXT u
  NEXT i
  mx6 = 0
  my6 = 0
 END IF
END IF
IF soobs7 > 0 THEN
 soobs7 = soobs7 - 1
 pos7 = 1
 LOCATE 1, 1
 PRINT "Идет разминирование..."
 nedvij = 1
ELSEIF soobs7 = 0 THEN
 IF pos7 = 1 THEN
  nedvij = 0
  LOCATE 1, 1
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  PUT (mx7, my7), mina, PSET
  pos7 = 0
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = mx7 AND xo(i, u, 2) = my7 THEN
     b(i, u) = "i"
    END IF
   NEXT u
  NEXT i
  mx7 = 0
  my7 = 0
 END IF
END IF
IF soobs8 > 0 THEN
 soobs8 = soobs8 - 1
 pos8 = 1
 LOCATE 1, 1
 PRINT "Идет разминирование..."
 nedvij = 1
ELSEIF soobs8 = 0 THEN
 IF pos8 = 1 THEN
  LOCATE 1, 1
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  PUT (mx8, my8), mina, PSET
  nedvij = 0
  pos8 = 0
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = mx8 AND xo(i, u, 2) = my8 THEN
     b(i, u) = "i"
    END IF
   NEXT u
  NEXT i
  mx8 = 0
  my8 = 0
 END IF
END IF
IF soobs9 > 0 THEN
 soobs9 = soobs9 - 1
 pos9 = 1
 LOCATE 1, 1
 PRINT "Идет разминирование..."
 nedvij = 1
ELSEIF soobs9 = 0 THEN
 IF pos9 = 1 THEN
  LOCATE 1, 1
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  PUT (mx9, my9), mina, PSET
  nedvij = 0
  pos9 = 0
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = mx9 AND xo(i, u, 2) = my9 THEN
     b(i, u) = "i"
    END IF
   NEXT u
  NEXT i
  mx9 = 0
  my9 = 0
 END IF
END IF
IF soobs1 > 0 THEN
 soobs1 = soobs1 - 1
 pos1 = 1
ELSEIF soobs1 = 0 THEN
 IF pos1 = 1 THEN
  LOCATE 1, 15
  PRINT "                "
  LINE (0, 0)-(639, 0), 11
  pos1 = 0
 END IF
END IF
IF soobs4 > 0 THEN
 soobs4 = soobs4 - 1
 pos4 = 1
ELSEIF soobs4 = 0 THEN
 IF pos4 = 1 THEN
  LOCATE 1, 1
  PRINT "                 "
  LINE (0, 0)-(639, 0), 11
  pos4 = 0
 END IF
END IF
IF soobs2 > 0 THEN
 soobs2 = soobs2 - 1
 pos2 = 1
ELSEIF soobs2 = 0 THEN
 IF pos2 = 1 THEN
  LOCATE 1, 10
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  pos2 = 0
 END IF
END IF
IF soobs5 > 0 THEN
 soobs5 = soobs5 - 1
 pos5 = 1
ELSEIF soobs5 = 0 THEN
 IF pos5 = 1 THEN
  LOCATE 1, 1
  PRINT "                      "
  LINE (0, 0)-(639, 0), 11
  pos5 = 0
 END IF
END IF
IF soobs3 > 0 THEN
 soobs3 = soobs3 - 1
 pos3 = 1
ELSEIF soobs3 = 0 THEN
 IF pos3 = 1 THEN
  LOCATE 1, 10
  PRINT "                  "
  LINE (0, 0)-(639, 0), 11
  pos3 = 0
 END IF
END IF
IF soobs6 > 0 OR soobs7 > 0 OR soobs8 > 0 OR soobs9 > 0 THEN
 LINE (620, 0)-(639, 0), 11
 IF soobs6 > 0 THEN
  LINE (621, 99 - 99 * (soobs6 / 600))-(638, 1), 0, BF
 ELSEIF soobs7 > 0 THEN
  LINE (621, 99 - 99 * (soobs7 / 600))-(638, 1), 0, BF
 ELSEIF soobs8 > 0 THEN
  LINE (621, 99 - 99 * (soobs8 / 600))-(638, 1), 0, BF
 ELSEIF soobs9 > 0 THEN
  LINE (621, 99 - 99 * (soobs9 / 600))-(638, 1), 0, BF
 END IF
END IF
IF esttime > 0 THEN
 esttime = esttime - 1
 estpok = 1
 LINE (620, 200)-(639, 200), 11
 LINE (621, 299 - 99 * (esttime / pervtime))-(638, 201), 0, BF
ELSE
 esttime = 0
 IF estpok = 1 THEN
  LINE (621, 201)-(638, 299), 0, BF
  LOCATE 1, 10
  PRINT "Монстры несъедобны"
  SOUND 300, 8
  soobs3 = 300
  estpok = 0
 END IF
END IF
LINE (620, 300)-(639, 300), 11
LINE (621, 399 - 99 * (life / alllife))-(638, 301), 0, BF
LINE (621, 399 - 99 * (life / alllife))-(638, 399), 10, BF
IF pokmoney <> 1 THEN
 LINE (620, 100)-(639, 100), 11
 IF allmon <> 0 THEN
  LINE (621, 199 - 99 * (money / allmon))-(638, 101), 0, BF
  LINE (621, 199 - 99 * (money / allmon))-(638, 199), 14, BF
 END IF
ELSE
 LINE (621, 101)-(638, 199), 14, BF
END IF
IF POINT(x2 + 5, y2 + 8) = 6 AND POINT(x2 + 5, y2 + 6) = 12 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(36) = minst(36) AND zo(125) = minst(125) AND zo(89) = minst(89) THEN
  life = life - 1
  LOCATE 1, 1
  PRINT "Вы подорвались на мине"
  soobs5 = 300
  SOUND 1500, 2
  SOUND 32000, 2
  SOUND 800, 2
  PUT (x2, y2), z2, PSET
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
     b(i, u) = "."
    END IF
   NEXT u
  NEXT i
 END IF
END IF
IF POINT(x2 + 5, y2 + 9) = 6 AND POINT(x2 + 5, y2 + 7) = 7 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(36) = mina(36) AND zo(125) = mina(125) AND zo(89) = mina(89) THEN
  LOCATE 1, 1
  PRINT "Вы подобрали мину"
  ocki = ocki + 3
  SOUND 2400, 1
  SOUND 32000, 5
  SOUND 1000, 2
  SOUND 32000, 1
  SOUND 1000, 2
  soobs4 = 300
  minnum = minnum + 1
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
     b(i, u) = "."
    END IF
   NEXT u
  NEXT i
  PUT (x2, y2), z2, PSET
 END IF
END IF
IF POINT(x2 + 4, y2) = 8 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(35) = smertmon(35) AND zo(100) = smertmon(100) AND zo(152) = smertmon(152) AND zo(60) = smertmon(60) AND zo(121) = smertmon(121) AND zo(15) = smertmon(15) THEN
  LOCATE 1, 10
  PRINT "Монстры стали съедобны"
  ocki = ocki + 20
  SOUND 1000, 2
  SOUND 32000, 2
  SOUND 1230, 2
  SOUND 32000, 2
  SOUND 850, 3
  esttime = esttime + 2500
  pervtime = esttime
  LINE (621, 201)-(638, 299), 12, BF
  soobs2 = 300
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
     b(i, u) = "."
    END IF
   NEXT u
  NEXT i
 END IF
END IF
IF POINT(x2 + 5, y2 + 9) = 6 AND POINT(x2 + 8, y2 + 6) = 3 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(56) = lodka(56) AND zo(33) = lodka(33) AND zo(165) = lodka(165) AND zo(45) = lodka(45) AND zo(78) = lodka(78) THEN
  LOCATE 1, 15
  ocki = ocki + 3
  PRINT "Вы нашли лодку!"
  lodkaind = 1
  SOUND 1000, 2
  SOUND 32000, 2
  SOUND 1200, 2
  soobs1 = 250
  FOR i = 1 TO v
   FOR u = 1 TO n
    IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
     b(i, u) = "."
    END IF
   NEXT u
  NEXT i
 END IF
END IF
monnewat = 0
IF POINT(x, y) = 9 OR POINT(x + 1, y + 1) = 9 THEN
 monnewat = 1
 FOR ioy = 1 TO waternum
  IF xwater(ioy) = x2 AND ywater(ioy) = y2 AND lodkaind = 0 THEN
   lodkawat = 0
   life = life - 1
   PUT (x, y), z3, XOR
   PUT (x2, y2), z3', OR
   cil = 1
   LOCATE 1, 15
   PRINT "Поздравляем Вас! Вам наконец-то удалось утопиться!"
   SOUND 1300, 2
   SLEEP 2
   LOCATE 1, 15
   PRINT "                                                  "
   LINE (0, 0)-(639, 0), 11
   IF life <= 0 THEN
    life = 0
    LOCATE 1, 1
    PRINT "Несчастная хряпочка совсем утопилась..."
    SLEEP
    END
   END IF
   PUT (x2, y2), z3, XOR
   PUT (oldx, oldy), z3
  ELSEIF xwater(ioy) = x2 AND ywater(ioy) = y2 AND lodkaind = 1 THEN
   IF lodkawat = 0 THEN
    watlod(ioy) = 1
    PUT (x, y), z3, XOR
    PUT (x2, y2), z3
    PUT (x2, y2), lodka
    lodkax = x2: lodkay = y2
    oldx = x: oldy = y
    x = x2: y = y2
    lodkawat = 1
    ind3 = ioy
    cil = 1
   ELSE
    watlod(ind3) = 0
    watlod(ioy) = 1
    ind3 = ioy
    PUT (x, y), z3, XOR
    PUT (x, y), lodka, XOR
    PUT (x2, y2), z3
    PUT (x2, y2), lodka
    lodkax = x2: lodkay = y2
    oldx = x: oldy = y
    x = x2: y = y2
    cil = 1
   END IF
  END IF
 NEXT ioy
ELSE
 lodkawat = 0
END IF
IF lodkawat = 1 AND (POINT(x, y) <> 9 AND POINT(x + 1, y + 1) <> 9) THEN
 watlod(ind3) = 0
END IF
IF POINT(x2 + 1, y2 + 1) = 7 THEN
 FOR ind1 = 1 TO waternum
  IF ledneled(ind1) = 1 THEN
   IF ledx(ind1) = x2 AND ledy(ind1) = y2 THEN
    PUT (x, y), z3, XOR
    PUT (x2, y2), z3
    oldx = x: oldy = y: x = x2: y = y2
    cil = 1
    hodled(ind1) = 1
   END IF
  END IF
 NEXT ind1
END IF
IF POINT(x2 + 1, y2 + 1) = 8 AND POINT(x2 + 3, y2 + 5) = 0 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(156) = res2(156) AND zo(45) = res2(45) AND zo(78) = res2(78) THEN
  FOR ui = 1 TO resnum
   IF xr(ui) = x AND yr(ui) = y AND rtrig(ui) = 0 AND ir(ui) >= resdvij - 10 THEN
    life = life - 1
    PUT (oldx, oldy), z3
    LOCATE 1, 15
    PRINT "В вас появилось несколько новых отверстий!"
    SOUND 2300, 2
    SLEEP 2
    LOCATE 1, 15
    PRINT "                                          "
    LINE (0, 0)-(639, 0), 11
    IF life <= 0 THEN
     LOCATE 1, 1
     PRINT "Несчастная хряпочка покончила с жизнью через накалывание на решетку"
     SLEEP
     END
    END IF
   END IF
  NEXT ui
 END IF
END IF
IF POINT(x2 + 1, y2 + 1) = 8 AND POINT(x2 + 3, y2 + 5) = 8 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 IF zo(156) = res1(156) AND zo(145) = res1(145) AND zo(78) = res1(78) THEN
  PUT (x, y), z3, XOR
  PUT (x, y), z3
  cil = 1
 END IF
END IF
IF POINT(x2 + 5, y2 + 1) = 13 OR POINT(x2, y2) = 11 OR POINT(x2 + 5, y2 + 5) = 12 OR POINT(x2 + 3, y2 + 3) = 14 OR POINT(x2, y2) = 4 OR POINT(x2 + 3, y2) = 9 OR (POINT(x2, y2) = 15 AND POINT(x2 + 3, y2) = 12) OR POINT(x2 + 1, y2 + 9) = 6 THEN
 GET (x2, y2)-(x2 + 9, y2 + 9), zo
 FOR l = 0 TO 200
  IF zo(l) = z4(l) THEN k = k + 1
  IF zo(l) = z5(l) THEN k1 = k + 1
  IF zo(l) = jizn(l) THEN k2 = k2 + 1
  IF zo(l) = klut(l) THEN k3 = k3 + 1
 NEXT l
'stena
 IF POINT(x2 + 1, y2 + 9) = 6 AND POINT(x2 + 2, y2 + 2) = 6 AND POINT(x2 + 2, y2 + 6) = 6 THEN
  IF zo(156) = stena(156) AND zo(34) = stena(34) AND zo(56) = stena(56) THEN
   PUT (x, y), z3, XOR
   PUT (x, y), z3
   LOCATE 1, 13
   PRINT "Вы не привидение, вы не можете проходить сквозь стены!"
   SOUND 500, 4
   SLEEP 2
   LOCATE 1, 13
   PRINT "                                                      "
   LINE (0, 0)-(639, 0), 11
   cil = 1
  END IF
 END IF
'бомба=взять
 IF POINT(x2, y2) = 15 AND POINT(x2 + 3, y2) = 12 THEN
  IF zo(156) = bomb(156) AND zo(34) = bomb(34) AND zo(76) = bomb(76) THEN
   x = x2: y = y2
   PUT (oldx, oldy), z3, XOR
   PUT (x, y), z3, PSET
   oldx = x: oldy = y
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   cil = 0
   kolvobomb = kolvobomb + 1
   ocki = ocki + 3
   SOUND 1300, 2
  END IF
 END IF
 IF POINT(x2 + 3, y2) = 9 THEN  'супермонета
  IF zo(56) = z7(56) AND zo(34) = z7(34) AND zo(156) = z7(156) THEN
    x = x2: y = y2
    PUT (oldx, oldy), z3, XOR
    PUT (x, y), z3, PSET
    oldx = x: oldy = y
    FOR i = 1 TO v
     FOR u = 1 TO n
      IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
       b(i, u) = "."
      END IF
     NEXT u
    NEXT i
    cil = 0
    money = money + 10
    ocki = ocki + 13
    SOUND 2300, 2
  END IF
 END IF
 IF POINT(x2, y2) = 4 THEN 'дверь
  IF zo(56) = dver(56) AND zo(134) = dver(134) AND zo(156) = dver(156) THEN
   IF kolvoklu > 0 THEN
    x = x2: y = y2
    PUT (oldx, oldy), z3, XOR
    PUT (x, y), z3, PSET
    oldx = x: oldy = y
    FOR i = 1 TO v
     FOR u = 1 TO n
      IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
       b(i, u) = "."
      END IF
     NEXT u
    NEXT i
    cil = 0
    kolvoklu = kolvoklu - 1
    ocki = ocki + 5
    FOR poik = 250 TO 1000 STEP 50
     SOUND poik, 1
    NEXT poik
   ELSE
    PUT (x, y), z3, XOR
    PUT (x, y), z3
    LOCATE 1, 13
    PRINT "У вас нет ключей!"
    SOUND 765, 4
    SLEEP 1
    LOCATE 1, 13
    PRINT "                 "
    LINE (0, 0)-(639, 0), 11
    cil = 1
   END IF
  END IF
 END IF
 IF POINT(x2 + 3, y2 + 3) = 14 THEN       'ключ
  IF zo(13) = klut(13) AND zo(135) = klut(135) THEN
   x = x2: y = y2
   PUT (oldx, oldy), z3, XOR
   PUT (x, y), z3, PSET
   oldx = x: oldy = y
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   cil = 0
   kolvoklu = kolvoklu + 1
   ocki = ocki + 1
   PLAY "L8AL8CL8B"
  END IF
 END IF

 IF POINT(x2 + 5, y2 + 5) = 12 THEN 'жизнь
  IF k2 / 200 > .9 THEN
  x = x2: y = y2
  PUT (oldx, oldy), z3, XOR
  PUT (x, y), z3, PSET
  oldx = x: oldy = y
   FOR i = 1 TO v
    FOR u = 1 TO n
     IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
      b(i, u) = "."
     END IF
    NEXT u
   NEXT i
   cil = 1
   life = life + 1
   ocki = ocki + 4
   alllife = life
   SOUND 2300, 5
  END IF
 END IF
 IF POINT(x2 + 1, y2 + 1) = 11 THEN 'выход
 IF zo(50) = z5(50) AND zo(75) = z5(75) AND zo(155) = z5(155) THEN
  IF money >= allmon THEN
   PUT (x, y), z3, XOR
   PUT (x2, y2), z3
   LOCATE 1, 1
   PRINT "Вы прошли лабиринт"
   SOUND 450, 2
   SOUND 32000, 2
   SOUND 1450, 2
   SOUND 32000, 2
   SOUND 1050, 2
   SOUND 32000, 2
   SOUND 1060, 4
   FOR i = 1 TO kolvomon
    IF dohlmon(i) = 1 THEN kozia = kozia + 1
   NEXT i
   ocki = ocki + (life / billife) * 100
   FOR i = 1 TO kozia
    ocki = ocki + 19
   NEXT i
   time2 = TIMER
   dtime = time2 - time1
   ocki = ocki + (n * v) / (dtime * 2)
   IF kozia = kolvomon THEN
    LOCATE 5, 10
    PRINT "Поздравляем Вас! Вы съели всех монстров"
    ocki = ocki + 22
   ELSE
    LOCATE 5, 10
    PRINT "Вы съели"; kozia; "монстров из"; kolvomon
   END IF
   LOCATE 12, 10
   PRINT "Вы заработали"; ocki; "очков"
   LOCATE 13, 10
   PRINT "Вы играли"; INT(dtime); "секунд"
   END
  ELSE
   PUT (x, y), z3, XOR
   PUT (x, y), z3
   LOCATE 1, 13
   PRINT "Вы собрали не все монеты!"
   SOUND 500, 4
   SLEEP 1
   LOCATE 1, 13
   PRINT "                         "
   LINE (0, 0)-(639, 0), 11
   cil = 1
  END IF
 END IF
 END IF
 IF monnewat = 0 THEN
  IF POINT(x2 + 5, y2 + 1) = 13 THEN
   IF k / 200 > .9 THEN
    x = x2: y = y2
    PUT (oldx, oldy), z3, XOR
    PUT (x, y), z3, PSET
    oldx = x: oldy = y
    FOR i = 1 TO v
     FOR u = 1 TO n
      IF xo(i, u, 1) = x2 AND xo(i, u, 2) = y2 THEN
       b(i, u) = "."
      END IF
     NEXT u
    NEXT i
    cil = 0
   END IF
   money = money + 1
   ocki = ocki + 2
   k = 0
   k1 = 0
   k2 = 0
   k3 = 0
   SOUND 2000, 2
  END IF
 END IF
END IF
IF pokmoney <> 1 THEN
 IF money >= allmon THEN
  pokmoney = 1
  LOCATE 1, 1
  PRINT "Вы собрали необходимое количество монет"
  SOUND 1000, 2
  SOUND 32000, 2
  SOUND 1500, 2
  SOUND 32000, 1
  SOUND 1750, 2
  SLEEP 1
  LOCATE 1, 1
  PRINT "                                        "
  LINE (0, 0)-(639, 0), 11
  cil = 0
 END IF
END IF
IF x2 >= 10 AND x2 <= 619 AND y2 >= 10 AND y2 <= 469 THEN
 IF cil = 0 THEN
 IF POINT(x2 + 5, y2 + 5) <> 1 THEN
  PUT (x, y), z3, XOR
  PUT (x2, y2), z3
  x = x2: y = y2
  oldx = x: oldy = y
 END IF
END IF
END IF
'IF menudvij = 0 THEN   'POINT(x2 + 5, y2 + 5) <> 1 and
 IF (x2 < 10 OR x2 > 619 OR y2 < 10 OR y2 > 469) OR menudvij = 0 THEN
  IF x2 > 619 THEN 'влево
   poprx = 320 - x2
   x = 320
   popry = 0
   poix = -10
   poiy = 0
  END IF
  IF x2 < 10 THEN 'вправо
   poprx = 320 - x2
   x = 320
   popry = 0
   poix = 10
   poiy = 0
  END IF
  IF y2 > 469 THEN 'вверх
   y = 240
   popry = 240 - y2
   poprx = 0
   poix = 0
   poiy = -10
  END IF
  IF y2 < 10 THEN      'вниз
   y = 240
   popry = 240 - y2
   poprx = 0
   poix = 0
   poiy = 10
  END IF
  CLS
kozz = 1
LINE (0, 0)-(0, 479), 11
LINE (0, 479)-(639, 479), 11
LINE (639, 479)-(639, 0), 11
LINE (639, 0)-(0, 0), 11
LINE (620, 200)-(639, 300), 11, B
LINE (620, 300)-(639, 400), 11, B
LINE (620, 100)-(639, 200), 11, B
LINE (620, 0)-(639, 100), 11, B
  FOR i = 1 TO v
   FOR u = 1 TO n
   xo(i, u, 1) = xo(i, u, 1) + poprx
   xo(i, u, 2) = xo(i, u, 2) + popry
   IF xo(i, u, 1) > 0 AND xo(i, u, 1) <= 619 AND xo(i, u, 2) >= 10 AND xo(i, u, 2) < 469 THEN
    IF b(i, u) = "*" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z1, PSET
    ELSEIF b(i, u) = "." THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z2, PSET
    ELSEIF b(i, u) = "m" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z4, PSET
    ELSEIF b(i, u) = "e" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z5, PSET
     xexit = xo(i, u, 1): yexit = xo(i, u, 2)
    ELSEIF b(i, u) = "j" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), jizn, PSET
    ELSEIF b(i, u) = "d" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), dver, PSET
    ELSEIF b(i, u) = "k" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), klut, PSET
    ELSEIF b(i, u) = "s" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), z7, PSET
    ELSEIF b(i, u) = "b" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), bomb, PSET
    ELSEIF b(i, u) = "S" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), stena, PSET
    ELSEIF b(i, u) = "L" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), lodka, PSET
    ELSEIF b(i, u) = "D" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), smertmon, PSET
    ELSEIF b(i, u) = "i" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), mina, PSET
    ELSEIF b(i, u) = "N" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), minst, PSET
    ELSEIF b(i, u) = "K" THEN
     PUT (xo(i, u, 1), xo(i, u, 2)), krov, PSET
    END IF
   END IF
  NEXT u
 NEXT i
IF pert1 = 0 THEN
 PUT (200, 460), per1, PSET
ELSE
 PUT (200, 460), per2, PSET
END IF
IF pert2 = 0 THEN
 PUT (215, 460), per1, PSET
ELSE
 PUT (215, 460), per2, PSET
END IF
IF POINT(x + 5, y + 5) = 0 AND POINT(x + 3, y + 3) = 0 AND POINT(x + 6, y + 7) = 0 THEN
 PUT (x, y), z3
ELSE
 x = poix + x: y = y + poiy
 PUT (x, y), z3
 x2 = x: y2 = y: oldx = x: oldy = y
 poix = 0: poiy = 0
END IF
END IF
'END IF
FOR g = 1 TO kolvomon
IF dohlmon(g) <> 1 THEN
 tah = 0
 xmon2(g) = xmon(g): ymon2(g) = ymon(g)
'проверка на перерисовку экрана
 IF (poprx <> 0 OR popry <> 0) OR menudvij = 0 THEN
  xmon(g) = xmon(g) + poprx
  ymon(g) = ymon(g) + popry
  '  bu(g, 6) = bu(g, 6) + poprx / 10
'  bu(g, 7) = bu(g, 7) + popry / 10
  IF xmon(g) > 0 AND xmon(g) < 619 AND ymon(g) > 0 AND ymon(g) < 469 THEN
   PUT (xmon(g), ymon(g)), z6
   tah = 1
  END IF
  xmon2(g) = xmon(g): ymon2(g) = ymon(g)
  xold2(g) = xmon(g): yold2(g) = ymon(g)
 END IF
 opa(g) = opa(g) + 1
 IF opa(g) = mondvij THEN
'определение сторон, в ктр может пойти монстр
  vibor = 0
  bu(g, 9) = bu(g, 1)
  bu(g, 1) = 0
  w = bu(g, 6): e = bu(g, 7)
'  IF b(w + 1, e) <> "*" THEN   'справа
'   bu(g, 1) = bu(g, 1) + 1
'   bu(g, 2) = 1
'  ELSE
'   bu(g, 2) = 0
'  END IF
'  IF b(w - 1, e) <> "*" THEN  'слева
'   bu(g, 1) = bu(g, 1) + 1
'   bu(g, 3) = 1
'  ELSE
'   bu(g, 3) = 0
'  END IF
'  IF b(w, e - 1) <> "*" THEN  'сверху
'   bu(g, 1) = bu(g, 1) + 1
'   bu(g, 4) = 1
'  ELSE
'   bu(g, 4) = 0
'  END IF
'  IF b(w, e + 1) <> "*" THEN 'снизу
'   bu(g, 1) = bu(g, 1) + 1
'   bu(g, 5) = 1
'  ELSE
'   bu(g, 5) = 0
'  END IF
'  w = 0: e = 0
'   LOCATE 1, 1
'   PRINT "l"
'   SOUND 1200, 1
'   LOCATE 1, 1
'   PRINT " "
'  vibor = 0
'  w = bu(g, 6): e = bu(g, 7)
'  IF xmon(g) > 10 AND xmon(g) < 629 AND ymon(g) < 469 AND ymon(g) > 0 THEN
   IF POINT(xmon(g) + 15, ymon(g) + 5) = 1 THEN
    bu(g, 2) = 0
   ELSE
   bu(g, 1) = bu(g, 1) + 1
    bu(g, 2) = 1
   END IF
   IF POINT(xmon(g) - 5, ymon(g) + 5) = 1 THEN
    bu(g, 3) = 0
   ELSE
   bu(g, 1) = bu(g, 1) + 1
    bu(g, 3) = 1
   END IF
   IF POINT(xmon(g) + 5, ymon(g) + 15) = 1 THEN
    bu(g, 4) = 0
   ELSE
   bu(g, 1) = bu(g, 1) + 1
    bu(g, 4) = 1
   END IF
   IF POINT(xmon(g) + 5, ymon(g) - 5) = 1 THEN
    bu(g, 5) = 0
   ELSE
   bu(g, 1) = bu(g, 1) + 1
    bu(g, 5) = 1
   END IF
'  END IF
  vibor = 0
  IF dxmon(g) = -10 AND ((POINT(xmon(g) - 5, ymon(g) + 5) = 1)) THEN vibor = 1
  IF dxmon(g) = 10 AND ((POINT(xmon(g) + 15, ymon(g) + 5) = 1)) THEN vibor = 1
  IF dymon(g) = -10 AND (((POINT(xmon(g) + 5, ymon(g) - 5) = 1))) THEN vibor = 1
  IF dymon(g) = 10 AND (((POINT(xmon(g) + 5, ymon(g) + 15) = 1))) THEN vibor = 1
  'выбор напрaвления нового движения
'  vibor = 0
  IF bu(g, bu(g, 8)) = 0 OR bu(g, 9) - bu(g, 1) <> 0 OR ss$ = "q" OR (dxmon(g) = 0 AND dymon(g) = 0) OR xmon(g) = xold2(g) OR ymon(g) = yold2(g) THEN vibor = 1
  IF vibor = 1 THEN
'   LOCATE 1, 1
'   PRINT "l"
'   SOUND 200, 1
'   LOCATE 1, 1
'   PRINT " "
   spr = 0
'   ert = 0
   ert = RND * (bu(g, 1))
   FOR pr = 2 TO 5
    IF bu(g, pr) = 1 THEN spr = spr + 1
    IF spr = ert THEN
     bu(g, 8) = pr
     'klp = pr
     pr = 5
    END IF
   NEXT pr
' '  IF klp = 2 AND bu(g, 8) = 3 AND bu(g, 1) > 2 THEN GOTO 115
' '  IF klp = 4 AND bu(g, 8) = 5 AND bu(g, 1) > 2 THEN GOTO 115
' '  bu(g, 8) = klp
   spr = 0: ert = 0
   'bu(g, 8) = 2 + RND * bu(g, 1)
   LOCATE 1, 1
   PRINT bu(g, 8)
   LOCATE 2, 1
   PRINT bu(g, 1)
' '  SLEEP
' '  w = bu(g, 6): e = bu(g, 7)
   IF bu(g, 8) = 2 AND bu(g, 2) = 1 THEN
    dxmon(g) = 10: dymon(g) = 0
'    bu(g, 6) = bu(g, 6) + 1
   ELSEIF bu(g, 8) = 3 AND bu(g, 3) = 1 THEN
    dxmon(g) = -10: dymon(g) = 0
'    bu(g, 6) = bu(g, 6) - 1
   ELSEIF bu(g, 8) = 4 AND bu(g, 4) = 1 THEN
    dxmon(g) = 0: dymon(g) = -10
'    bu(g, 7) = bu(g, 7) - 1
   ELSEIF bu(g, 8) = 5 AND bu(g, 5) = 1 THEN
    dxmon(g) = 0: dymon(g) = 10
'    bu(g, 7) = bu(g, 7) + 1
   ELSE
    dxmon(g) = 0: dymon(g) = 0
   END IF
  END IF
  xmon(g) = xmon(g) + dxmon(g)
  ymon(g) = ymon(g) + dymon(g)
'  IF xold2(g) <> xmon(g) OR yold2(g) <> ymon(g) THEN
'  IF (dxmon(g) + xmon(g) > 10 AND dxmon(g) + xmon(g) < 629 AND dymon(g) + ymon(g) < 469 AND dymon(g) + ymon(g) > 0 AND POINT(xmon(g) + dxmon(g), ymon(g) + dymon(g)) <> 1) OR b(bu(g, 6) + dxmon \ 10, bu(g, 7) + dymon \ 10) <> "*" THEN
'   IF dxmon(g) = 10 THEN bu(g, 6) = bu(g, 6) + 1
'   IF dxmon(g) = -10 THEN bu(g, 6) = bu(g, 6) - 1
'   IF dymon(g) = 10 THEN bu(g, 7) = bu(g, 7) + 1
'   IF dxmon(g) = -10 THEN bu(g, 7) = bu(g, 7) - 1
'  END IF
 
  xmon2(g) = xmon(g): ymon2(g) = ymon(g)

'  bu(g, 6) = bu(g, 6) + dxmon(g) / 10
'  bu(g, 7) = bu(g, 7) + dymon(g) / 10
  opa(g) = 0
  boom = 0
  lum = 0
  IF POINT(xmon(g) + 5, ymon(g) + 5) = 1 AND POINT(xmon(g) + 6, ymon(g) + 5) = 1 THEN
   xmon(g) = xold2(g)
   xmon2(g) = xold2(g)
   ymon(g) = yold2(g)
   ymon2(g) = yold2(g)
  END IF
'  IF ((POINT(xmon2(g) + 5, ymon2(g) + 8) = 6 AND POINT(xmon2(g) + 5, ymon2(g) + 6) = 12) OR (POINT(xmon2(g) + 1, ymon2(g) + 8) = 0 AND POINT(xmon2(g) + 5, ymon2(g) + 5) = 0 AND POINT(xmon2(g), ymon2(g)) = 0 AND POINT(xmon2(g) + 5, ymon2(g) + 1) = 0 AND POINT(xmon2(g) + 1, ymon2(g) + 0) = 0) OR (POINT(xmon2(g) + 4, ymon2(g)) = 14 AND POINT(xmon2(g), ymon2(g) + 5) = 14) OR POINT(xmon2(g) + 4, ymon2(g) + 2) = 7) AND xmon2(g) > 0 AND xmon2(g) < 619 AND ymon2(g) > 0 AND ymon2(g) < 469 THEN
   IF xmon(g) > 0 AND xmon(g) < 619 AND ymon(g) > 0 AND ymon(g) < 469 THEN
    IF xold2(g) <= 0 OR xold2(g) >= 619 OR yold2(g) <= 0 OR yold2(g) >= 469 THEN
     IF tah = 0 THEN
      xmon(g) = xmon2(g): ymon(g) = ymon2(g)
      PUT (xmon(g), ymon(g)), z6
     END IF
    ELSE
     IF tah = 0 THEN
      PUT (xold2(g), yold2(g)), z6, XOR
      xmon(g) = xmon2(g): ymon(g) = ymon2(g)
      PUT (xmon(g), ymon(g)), z6
     END IF
    END IF
   END IF
  'ELSE
'   IF xmon(g) > 0 AND xmon(g) < 619 AND ymon(g) > 0 AND ymon(g) < 469 THEN
'    IF xold2(g) <= 0 OR xold2(g) >= 619 OR yold2(g) <= 0 OR yold2(g) >= 469 THEN
'     IF tah = 0 THEN
'      PUT (xmon(g), ymon(g)), z6
'     END IF
'    ELSE
'     IF tah = 0 THEN
'      PUT (xold2(g), yold2(g)), z6, XOR
'      PUT (xmon(g), ymon(g)), z6
'     END IF
'    END IF
'   END IF
'  END IF
  IF round = -10 THEN
   rastx = ABS(xmon(g) - x)
   rasty = ABS(ymon(g) - y)
   opa(g) = 0
   boom = 0
   lum = 0
   IF kodik1 <> code1 THEN
    IF xmon2(g) < 619 AND xmon2(g) > 0 AND ymon2(g) < 469 AND ymon2(g) > 0 THEN
     IF ABS(xmon2(g) - 10 - x) < rastx THEN
      IF ((POINT(xmon2(g) - 10 + 5, ymon2(g) + 8) = 6 AND POINT(xmon2(g) - 10 + 5, ymon2(g) + 6) = 12) OR POINT(xmon2(g) - 10 + 1, ymon2(g) + 1) = 7 OR (POINT(xmon2(g) - 10 + 1, ymon2(g) + 8) = 0 AND POINT(xmon2(g) - 10 + 5, ymon2(g) + 5) = 0 AND  _
POINT(xmon2(g) - 10, ymon2(g)) = 0 AND POINT(xmon2(g) - 10 + 5, ymon2(g) + 1) = 0 AND POINT(xmon2(g) - 10 + 1, ymon2(g) + 0) = 0) OR (POINT(xmon2(g) - 10 + 4, ymon2(g)) = 14 AND POINT(xmon2(g) - 10, ymon2(g) + 5) = 14)) THEN
       xmon2(g) = xmon2(g) - 10
       boom = 1
      END IF
     END IF
     IF ABS(xmon2(g) + 10 - x) < rastx THEN
      IF ((POINT(xmon2(g) + 15, ymon2(g) + 8) = 6 AND POINT(xmon2(g) + 15, ymon2(g) + 6) = 12) OR POINT(xmon2(g) + 10 + 1, ymon2(g) + 1) = 7 OR (POINT(xmon2(g) + 10 + 1, ymon2(g) + 8) = 0 AND POINT(xmon2(g) + 10 + 5, ymon2(g) + 5) = 0 AND POINT( _
xmon2(g) + 10, ymon2(g)) = 0 AND POINT(xmon2(g) + 10 + 5, ymon2(g) + 1) = 0 AND POINT(xmon2(g) + 10 + 1, ymon2(g) + 0) = 0) OR (POINT(xmon2(g) + 10 + 4, ymon2(g)) = 14 AND POINT(xmon2(g) + 10, ymon2(g) + 5) = 14)) THEN
       xmon2(g) = xmon2(g) + 10
       boom = 1
      END IF
     END IF
     IF ABS(ymon2(g) + 10 - y) < rasty THEN
      IF ((POINT(xmon2(g) + 5, ymon2(g) + 18) = 6 AND POINT(xmon2(g) + 5, ymon2(g) + 16) = 12) OR POINT(xmon2(g) + 1, ymon2(g) + 11) = 7 OR (POINT(xmon2(g) + 1, ymon2(g) + 18) = 0 AND POINT(xmon2(g) + 5, ymon2(g) + 15) = 0 AND POINT(xmon2(g), ymon2( _
g) + 10) = 0 AND POINT(xmon2(g) + 5, ymon2(g) + 10 + 1) = 0 AND POINT(xmon2(g) + 1, ymon2(g) + 10) = 0) OR (POINT(xmon2(g) + 4, ymon2(g) + 10) = 14 AND POINT(xmon2(g), ymon2(g) + 10 + 5) = 14)) THEN
       ymon2(g) = ymon2(g) + 10
       boom = 1
      END IF
     END IF
     IF ABS(ymon2(g) - 10 - y) < rasty THEN
      IF ((POINT(xmon2(g) + 5, ymon2(g) - 2) = 6 AND POINT(xmon2(g) + 5, ymon2(g) - 4) = 12) OR POINT(xmon2(g) + 1, ymon2(g) - 9) = 7 OR (POINT(xmon2(g) + 1, ymon2(g) - 2) = 0 AND POINT(xmon2(g) + 5, ymon2(g) - 5) = 0 AND POINT(xmon2(g), ymon2(g) -  _
10) = 0 AND POINT(xmon2(g) + 5, ymon2(g) - 9) = 0 AND POINT(xmon2(g) + 1, ymon2(g) - 10) = 0) OR (POINT(xmon2(g) + 4, ymon2(g) - 10) = 14 AND POINT(xmon2(g), ymon2(g) - 5) = 14)) THEN
       ymon2(g) = ymon2(g) - 10
       boom = 1
      END IF
     END IF
    END IF
   ELSE
    boom = 0
   END IF
   IF boom = 0 THEN
    boom = 0
    fsb = RND
    IF fsb < .25 THEN 'влево
     xmon2(g) = xmon2(g) - 10
    ELSEIF fsb < .5 THEN 'вправо
     xmon2(g) = xmon2(g) + 10
    ELSEIF fsb < .75 THEN 'вниз
     ymon2(g) = ymon2(g) + 10
    ELSEIF fsb < 1 THEN 'вверх
     ymon2(g) = ymon2(g) - 10
    END IF
   END IF
  END IF
 END IF
  xold2(g) = xmon(g): yold2(g) = ymon(g)

END IF

IF (POINT(xmon2(g) + 5, ymon2(g) + 8) = 12 AND POINT(xmon2(g) + 5, ymon2(g) + 6) = 6) THEN
 dohlmon(g) = 1
 xmon(g) = 0
 ymon(g) = 0
 SOUND 200, 2
 PUT (xmon2(g), ymon2(g)), z2, PSET
 FOR radius = 1 TO 4
  CIRCLE (xmon2(g) + 4.5, ymon2(g) + 4.5), radius, 12
  SOUND 32000, 2
  SOUND 300, 1
 NEXT radius
 PUT (xmon2(g), ymon2(g)), krov, PSET
 FOR i = 1 TO v
  FOR u = 1 TO n
   IF xo(i, u, 1) = xmon2(g) AND xo(i, u, 2) = ymon2(g) THEN
'    PUT (xmon2(g), ymon2(g)), krov, PSET
    b(i, u) = "K"
   END IF
  NEXT u
 NEXT i
END IF
NEXT g
'Проверка на столкновение с монстром
g = 0
IF POINT(x, y + 7) = 4 THEN
 FOR klm = 1 TO kolvomon
  IF x = xmon(klm) AND y = ymon(klm) THEN
   IF esttime = 0 THEN
'    PUT (x, y), z3, PSET
    xmon(klm) = 0
    ymon(klm) = 0
    dohlmon(klm) = 1
    IF RND < .75 THEN
     life = life - 1
     LOCATE 1, 13
     PRINT "Если я не ошибаюсь, вас немного скушали..."
     FOR uop = 1500 TO 300 STEP -300
      SOUND uop, 1
     NEXT uop
     PUT (x, y), krov, PSET
     PUT (x, y), z3
     FOR i = 1 TO v
      FOR u = 1 TO n
       IF xo(i, u, 1) = x AND xo(i, u, 2) = y THEN b(i, u) = "K"
      NEXT u
     NEXT i
     SLEEP 2
     LOCATE 1, 13
     PRINT "                                          "
    ELSE
     life = life - 5
     LOCATE 1, 13
     PRINT "Если я не ошибаюсь, вас сильно обкусали, но выплюнули..."
     FOR suop = 1800 TO 300 STEP -150
      SOUND suop, 1
     NEXT suop
     PUT (x, y), krov, PSET
     PUT (x, y), z3
     FOR i = 1 TO v
      FOR u = 1 TO n
       IF xo(i, u, 1) = x AND xo(i, u, 2) = y THEN b(i, u) = "K"
      NEXT u
     NEXT i
     SLEEP 2
     LOCATE 1, 13
     PRINT "                                                        "
    END IF
    IF life <= 0 THEN
     LOCATE 1, 1
     PRINT "Вас целиком и полностью съели злобные монстры..."
     LOCATE 1, 70
     PRINT "Жизней:"; life
     SOUND 500, 3
     SOUND 32000, 2
     SOUND 400, 3
     SOUND 32000, 2
     SOUND 200, 5
     PUT (x, y), krov, PSET
     PUT (x, y), z3
     FOR i = 1 TO v
      FOR u = 1 TO n
       IF xo(i, u, 1) = x AND xo(i, u, 2) = y THEN b(i, u) = "K"
      NEXT u
     NEXT i
     SLEEP
     END
    END IF
   ELSEIF esttime > 0 THEN
    PUT (x, y), krov, PSET
    PUT (x, y), z3
    FOR i = 1 TO v
     FOR u = 1 TO n
      IF xo(i, u, 1) = x AND xo(i, u, 2) = y THEN b(i, u) = "K"
     NEXT u
    NEXT i
    xmon(klm) = 0
    ymon(klm) = 0
    dohlmon(klm) = 1
    SOUND 2000, 2
    SOUND 32000, 2
    SOUND 2100, 2
    SOUND 32000, 2
    SOUND 2200, 2
   END IF
  END IF
 NEXT klm
END IF
FOR ui = 1 TO resnum
 IF dohres(ui) = 0 THEN
  ir(ui) = ir(ui) + 1
  IF poprx <> 0 OR popry <> 0 THEN
   xr(ui) = xr(ui) + poprx
   yr(ui) = yr(ui) + popry
   IF rtrig(ui) = 0 THEN
    IF xr(ui) > 0 AND xr(ui) < 619 AND yr(ui) > 10 AND yr(ui) < 479 THEN
     PUT (xr(ui), yr(ui)), res1
    END IF
   ELSEIF rtrig(ui) = 1 THEN
    IF xr(ui) > 0 AND xr(ui) < 619 AND yr(ui) > 10 AND yr(ui) < 479 THEN
     PUT (xr(ui), yr(ui)), res2
    END IF
   END IF
  END IF
  IF ir(ui) = resdvij THEN
   ir(ui) = 0
   IF rtrig(ui) = 0 THEN 'опускается
    IF xr(ui) > 0 AND xr(ui) < 619 AND yr(ui) > 10 AND yr(ui) < 479 THEN
     PUT (xr(ui), yr(ui)), res1, PSET
     rtrig(ui) = 1
    END IF
   ELSEIF rtrig(ui) = 1 THEN 'поднимается
    IF xr(ui) > 0 AND xr(ui) < 619 AND yr(ui) > 10 AND yr(ui) < 479 THEN
     PUT (xr(ui), yr(ui)), res2, PSET
     rtrig(ui) = 0
    END IF
   END IF
  END IF
 END IF
NEXT ui
IF lodkax <> x2 AND lodkay <> y2 THEN watlod(ind3) = 0
FOR wai = 1 TO waternum
 inwat(wai) = inwat(wai) + 1
 IF poprx <> 0 OR popry <> 0 OR menudvij = 0 THEN
  xwater(wai) = xwater(wai) + poprx
  ywater(wai) = ywater(wai) + popry
  ledx(wai) = ledx(wai) + poprx
  ledy(wai) = ledy(wai) + popry
  IF watlod(wai) = 0 THEN
   IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
    IF watertrig(wai) = 0 THEN
     PUT (xwater(wai), ywater(wai)), voda2, PSET
    ELSEIF watertrig(wai) = 1 THEN
     PUT (xwater(wai), ywater(wai)), voda1, PSET
    ELSEIF watertrig(wai) = 2 THEN
     PUT (xwater(wai), ywater(wai)), voda3, PSET
    END IF
   END IF
  END IF
  IF watertrig(wai) = 3 THEN
   IF ledx(wai) > 0 AND ledx(wai) < 619 AND ledy(wai) > 10 AND ledy(wai) < 469 THEN
    PUT (ledx(wai), ledy(wai)), led, PSET
   END IF
  END IF
 END IF
 IF inwat(wai) = vodadvij THEN
  inwat(wai) = 0
  IF watlod(wai) = 0 THEN
   IF watertrig(wai) = 0 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda2, PSET
      watertrig(wai) = 1
    END IF
   ELSEIF watertrig(wai) = 1 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda1, PSET
     watertrig(wai) = 2
    END IF
   ELSEIF watertrig(wai) = 2 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda3, PSET
     watertrig(wai) = 0
    END IF
   END IF
  END IF
 END IF
 IF ledneled(wai) = 1 AND hodled(wai) = 1 THEN
  ledtime(wai) = ledtime(wai) - 1
  IF ledtime(wai) = 0 THEN
   ledneled(wai) = 0
   hodled(wai) = 0
   FOR ind2 = 1 TO waternum
    IF watertrig(ind2) <> 3 THEN
     indlv = watertrig(ind2)
    END IF
   NEXT ind2
   xwater(wai) = ledx(wai)
   ywater(wai) = ledy(wai)
   watertrig(wai) = indlv
   inwat(wai) = inwat(indlv)
   IF indlv = 0 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda2, PSET
     watertrig(wai) = 1
    END IF
   ELSEIF watertrig(wai) = 1 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda1, PSET
     watertrig(wai) = 2
    END IF
   ELSEIF watertrig(wai) = 2 THEN
    IF xwater(wai) > 0 AND xwater(wai) < 619 AND ywater(wai) > 10 AND ywater(wai) < 469 THEN
     PUT (xwater(wai), ywater(wai)), voda3, PSET
     watertrig(wai) = 0
    END IF
   END IF
  END IF
 END IF
NEXT wai
'FOR d = 1 TO sarnum
' opesar(d) = opesar(d) + 1
' IF dohsar(d) = 0 THEN
'  IF (poprx <> 0 OR popry <> 0) OR menudvij = 0 THEN
'   osarx(d) = sarx(d)
'   osary(d) = sary(d)
'   sarx(d) = sarx(d) + poprx
'   sary(d) = sary(d) + popry
'  END IF
'  IF opesar(d) = 20 THEN
'   opesar(d) = 0
'    IF  THEN
'     saryv(d) = 10
'     sarxv(d) = 0
'     sav(d) = 2
'     ELSEIF xo(i, u, 1) = sarx(d) AND xo(i, u, 2) = sary(d) AND b(i, u + 1) = "*" THEN
'     sarxv(d) = -10
'      sav(d) = 1
'     ELSEIF xo(i, u, 1) = sarx(d) AND xo(i, u, 2) = sary(d) AND b(i, u - 1) = "*" THEN
'      sarxv(d) = 10
'      sav(d) = 1
'     ELSEIF sav(d) = 2 AND xo(i, u, 1) = sarx(d) AND xo(i, u, 2) = sary AND b(i + 1, u) = "*" THEN
'       sav(d) = 0
'     ELSEIF sav(d) = 0 THEN
'      sav(d) = 1
'      IF RND > .5 THEN
'       sarxv(d) = 10
'      ELSE
'       sarxv(d) = -10
'      END IF
'     END IF
'    NEXT u
'   NEXT i
'   sarx(d) = sarx(d) + sarxv(d)
'   sary(d) = sary(d) + saryv(d)
'   IF sarx(d) > 0 AND sarx(d) < 619 AND sary(d) > 10 AND sary(d) < 479 THEN
'    PUT (osarx(d), osary(d)), sar, XOR
'    PUT (sarx(d), sary(d)), sar
'   END IF
'   osarx(d) = sarx(d)
'   osary(d) = sary(d)
'   saryv(d) = 0
'  END IF
' END IF
'NEXT d
IF kozz = 1 THEN
 menudvij = 1
END IF
'poprx = 0: popry = 0
LOOP
END

SUB loadsprites
LINE (0, 0)-(9, 9), 1, BF
SHARED z1() AS INTEGER
GET (0, 0)-(9, 9), z1
LINE (0, 0)-(9, 9), 0, BF
SHARED z2() AS INTEGER
GET (0, 0)-(9, 9), z2
CIRCLE (5, 5), 5, 14, , , 1
PAINT (5, 5), 14, 14
CIRCLE (3, 3), 1, 0
CIRCLE (7, 3), 1, 0
CIRCLE (5, 3), 5, 0, 4 * 3.1415 / 3, 5 * 3.1415 / 3
SHARED z3() AS INTEGER
GET (0, 0)-(9, 9), z3
CLS
CIRCLE (5, 5), 5, 13, , , 1
PAINT (5, 5), 13, 13
LINE (3, 2)-(3, 8), 0
CIRCLE (7, 5), 3, 0, , , 2
SHARED z4() AS INTEGER
GET (0, 0)-(10, 10), z4
CLS
LINE (0, 0)-(9, 9), 11, BF
LINE (1, 1)-(8, 8), 0, BF
LINE (1, 1)-(7, 3), 11
LINE (7, 3)-(7, 6), 11
LINE (7, 6)-(1, 7), 11
PAINT (5, 5), 11, 11
SHARED z5() AS INTEGER
GET (0, 0)-(9, 9), z5
CLS
SHARED z6() AS INTEGER
LINE (0, 2)-(0, 9), 10
LINE (0, 2)-(2, 0), 10
LINE (2, 0)-(7, 0), 10
LINE (7, 0)-(9, 2), 10
LINE (9, 2)-(9, 9), 10
LINE (0, 9)-(3, 8), 10
LINE (2, 8)-(5, 9), 10
LINE (5, 9)-(7, 8), 10
LINE (7, 8)-(9, 9), 10
PAINT (5, 5), 10, 10
CIRCLE (3, 3), 1, 12
CIRCLE (7, 3), 1, 12
PAINT (3, 3), 12, 12
PAINT (7, 3), 12, 12
GET (0, 0)-(9, 9), z6
CLS
LINE (2.5, 0)-(5, 2.5), 12
LINE (5, 2.5)-(7.5, 0), 12
LINE (7.5, 0)-(9, 2.5), 12
LINE (9, 2.5)-(5, 9), 12
LINE (5, 9)-(0, 2.5), 12
LINE (0, 2.5)-(2.5, 0), 12
PAINT (5, 5), 12, 12
SHARED jizn() AS INTEGER
GET (0, 0)-(9, 9), jizn
CLS
LINE (0, 0)-(9, 9), 4, BF
LINE (1, 1)-(1, 2), 15
LINE (1, 7)-(1, 8), 15
CIRCLE (7, 4), 1, 15
PAINT (7, 4), 15, 15
PSET (7, 6), 15
SHARED dver() AS INTEGER
GET (0, 0)-(9, 9), dver
CLS
LINE (0, 3)-(5, 3), 14
LINE (0, 2)-(5, 2), 14
CIRCLE (7, 2.5), 2, 14
LINE (0, 3)-(0, 4), 14
LINE (2, 3)-(2, 5), 14
SHARED klut() AS INTEGER
GET (0, 0)-(9, 9), klut
CLS
LINE (3, 0)-(6, 0), 9
LINE (6, 0)-(9, 3), 9
LINE (9, 3)-(9, 6), 9
LINE (9, 6)-(6, 9), 9
LINE (6, 9)-(3, 9), 9
LINE (3, 9)-(0, 6), 9
LINE (0, 6)-(0, 3), 9
LINE (3, 0)-(0, 3), 9
PAINT (5, 5), 3, 9
LINE (3, 2)-(3, 6), 0
CIRCLE (6.5, 4.5), 2, 0, , , 2
SHARED z7() AS INTEGER
GET (0, 0)-(9, 9), z7
LINE (0, 0)-(9, 9), 15, BF
CIRCLE (4.5, 6), 3, 0
PAINT (5, 6), 0, 0
LINE (4.5, 1)-(4.5, 5), 0
LINE (4.5, 2)-(3, 0), 12
LINE (4.5, 2)-(5, 0), 12
LINE (4.5, 2)-(6, 0), 12
LINE (4.5, 2)-(4, 0), 12
PSET (3, 6), 7
CIRCLE (3, 6), 1, 8
SHARED bomb() AS INTEGER
GET (0, 0)-(9, 9), bomb
CLS
CIRCLE (2, 2), 2, 6
CIRCLE (2, 6), 2, 6
CIRCLE (7, 2), 2, 6
CIRCLE (7, 6), 2, 6
LINE (0, 9)-(9, 9), 6
PAINT (2, 2), 6, 6
PAINT (2, 6), 6, 6
PAINT (7, 2), 6, 6
PAINT (7, 6), 6, 6
SHARED stena() AS INTEGER
GET (0, 0)-(9, 9), stena
CLS
LINE (1, 0)-(1, 9), 8
LINE (3, 0)-(3, 9), 8
LINE (5, 0)-(5, 9), 8
LINE (7, 0)-(7, 9), 8
LINE (9, 0)-(9, 9), 8
LINE (0, 1)-(9, 1), 8
LINE (0, 7)-(9, 7), 8
SHARED res1() AS INTEGER
GET (0, 0)-(9, 9), res1
CLS
LINE (1, 0)-(1, 3), 8
LINE (3, 0)-(3, 3), 8
LINE (5, 0)-(5, 3), 8
LINE (7, 0)-(7, 3), 8
LINE (9, 0)-(9, 3), 8
LINE (0, 0)-(9, 0), 8
SHARED res2() AS INTEGER
GET (0, 0)-(9, 9), res2
CLS
LINE (0, 0)-(9, 9), 9, BF
LINE (0, 3)-(3, 0), 3
LINE (0, 6)-(6, 0), 3
LINE (0, 9)-(9, 0), 3
LINE (3, 9)-(9, 3), 3
LINE (6, 9)-(9, 6), 3
SHARED voda1() AS INTEGER
GET (0, 0)-(9, 9), voda1
CLS
LINE (0, 0)-(9, 9), 9, BF
LINE (0, 2)-(2, 0), 3
LINE (0, 5)-(5, 0), 3
LINE (0, 8)-(8, 0), 3
LINE (2, 9)-(9, 2), 3
LINE (5, 9)-(9, 5), 3
LINE (8, 9)-(9, 8), 3
SHARED voda2() AS INTEGER
GET (0, 0)-(9, 9), voda2
CLS
LINE (0, 0)-(9, 9), 9, BF
LINE (0, 1)-(1, 0), 3
LINE (0, 4)-(4, 0), 3
LINE (0, 7)-(7, 0), 3
LINE (1, 9)-(9, 1), 3
LINE (4, 9)-(9, 4), 3
LINE (7, 9)-(9, 7), 3
SHARED voda3() AS INTEGER
GET (0, 0)-(9, 9), voda3
CLS
LINE (0, 0)-(9, 9), 7, BF
PSET (2, 3), 11
PSET (6, 7), 11
SHARED led() AS INTEGER
GET (0, 0)-(9, 9), led
CLS
LINE (0, 7)-(9, 7), 6
CIRCLE (5, 7), 5, 6, 3.5, 6, 4 / 10
PAINT (5, 8), 6, 6
PSET (0, 8), 0
LINE (8, 6)-(9, 7), 3, BF
LINE (8, 7)-(9, 9), 3
SHARED lodka() AS INTEGER
GET (0, 0)-(9, 9), lodka
CLS
CIRCLE (4.5, 4), 4, 8
CIRCLE (3, 3), 1, 8
CIRCLE (6.5, 3), 1, 8
PAINT (5, 2), 8, 8
PAINT (5, 6), 8, 8
LINE (4, 5)-(3, 6), 12
LINE (4, 5)-(5, 6), 12
LINE (5, 6)-(3, 6), 12
PAINT (3, 3), 10, 8
PAINT (6.5, 3), 10, 8
PSET (2, 2), 8
SHARED smertmon() AS INTEGER
GET (0, 0)-(9, 9), smertmon
'CLS
'CIRCLE (4.5, 4.5), 4.5, 5
'PAINT (5, 5), 5, 5
'SHARED sar() AS INTEGER
'GET (0, 0)-(9, 9), sar
CLS
LINE (0, 8)-(9, 9), 6, BF
LINE (3, 7)-(6, 7), 7
SHARED mina() AS INTEGER
GET (0, 0)-(9, 9), mina
CLS
LINE (0, 8)-(9, 9), 6, BF
LINE (3, 6)-(6, 7), 12, BF
SHARED minst() AS INTEGER
GET (0, 0)-(9, 9), minst
CLS
SHARED per1() AS INTEGER
LINE (0, 0)-(9, 19), 10, B
LINE (1, 1)-(8, 18), 10, B
LINE (2, 2)-(7, 17), 10, B
LINE (3, 9)-(6, 2), 12, BF
GET (0, 0)-(9, 19), per1
CLS
SHARED per2() AS INTEGER
LINE (0, 0)-(9, 19), 10, B
LINE (1, 1)-(8, 18), 10, B
LINE (2, 2)-(7, 17), 10, B
LINE (3, 9)-(6, 17), 12, BF
GET (0, 0)-(9, 19), per2
CLS
SHARED krov() AS INTEGER
CIRCLE (4.5, 4.5), 3, 12
PAINT (4.5, 4.5), 12, 12
CIRCLE (7, 2.5), 2, 12
PAINT (7, 2.5), 12, 12
PSET (7, 4), 0
PSET (8, 4), 0
PSET (5, 1), 0
CIRCLE (4, 5), 1, 10
PSET (4, 5), 10
GET (0, 0)-(9, 9), krov
END SUB


SUB menu STATIC
SHARED kodik1 AS LONG
SHARED kodik2 AS LONG
SHARED menupokaz AS INTEGER
SHARED menubil AS INTEGER
SHARED menudvij AS INTEGER
SHARED lihster AS INTEGER
SHARED jel AS INTEGER
SHARED mondvij AS INTEGER
SHARED z3() AS INTEGER
IF menupokaz = 1 THEN
 GOTO 130
END IF
LINE (220, 120)-(420, 160), 10, B
LINE (220, 180)-(420, 220), 10, B
LINE (220, 240)-(420, 280), 10, B
LINE (220, 300)-(420, 340), 10, B
LINE (220, 360)-(420, 400), 10, B
LOCATE 18, 39
PRINT "ИГРА"
LOCATE 25, 38
PRINT "СПРАВКА"
LOCATE 33, 36
PRINT "УСТАНОВКИ"
LOCATE 41, 39
PRINT "ВЫХОД"
LINE (250, 130)-(390, 150), 10, B
LINE (250, 190)-(390, 210), 10, B
LINE (250, 250)-(390, 270), 10, B
LINE (250, 310)-(390, 330), 10, B
LINE (250, 370)-(390, 390), 10, B
PAINT (230, 130), 14, 10
PAINT (230, 190), 14, 10
PAINT (230, 250), 14, 10
PAINT (230, 310), 14, 10
PAINT (230, 370), 5, 10
DIM kn1(2150) AS INTEGER
DIM kn2(2150) AS INTEGER
DIM kn3(2150) AS INTEGER
DIM kn4(2150) AS INTEGER
DIM etakn(2150) AS INTEGER
GET (220, 120)-(420, 160), kn1
GET (220, 180)-(420, 220), kn2
GET (220, 240)-(420, 280), kn3
GET (220, 300)-(420, 340), kn4
GET (220, 360)-(420, 400), etakn

130 CLS

LINE (0, 0)-(639, 479), 9, B
LINE (1, 1)-(638, 478), 9, B
LINE (2, 2)-(637, 477), 9, B
PUT (225, 115), kn1
PUT (225, 175), kn2
PUT (225, 235), kn3
PUT (225, 295), kn4
PUT (225, 115), etakn
LINE (220, 120)-(220, 160), 10
LINE (220, 160)-(420, 160), 10
LINE (220, 120)-(225, 115), 10
LINE (220, 160)-(225, 155), 10
LINE (420, 160)-(425, 155), 10

LINE (220, 180)-(220, 220), 10
LINE (220, 220)-(420, 220), 10
LINE (220, 180)-(225, 175), 10
LINE (220, 220)-(225, 215), 10
LINE (420, 220)-(425, 215), 10

LINE (220, 240)-(220, 280), 10
LINE (220, 280)-(420, 280), 10
LINE (220, 240)-(225, 235), 10
LINE (220, 280)-(225, 275), 10
LINE (420, 280)-(425, 275), 10

LINE (220, 300)-(220, 340), 10
LINE (220, 340)-(420, 340), 10
LINE (220, 300)-(225, 295), 10
LINE (220, 340)-(225, 335), 10
LINE (420, 340)-(425, 335), 10
xmenu = 225: ymenu = 115: dmenu = 60
xmenu2 = 225: ymenu2 = 115
LOCATE 58, 22
PRINT "Для выбора пункта меню нажмите на ENTER"
DO
opr$ = INKEY$
IF opr$ <> "" THEN
 IF MID$(opr$, 2, 1) = "H" THEN 'вверх
  ymenu2 = ymenu2 - dmenu
 ELSEIF MID$(opr$, 2, 1) = "P" THEN 'вниз
  ymenu2 = ymenu2 + dmenu
 ELSEIF opr$ = CHR$(27) THEN
  END
 ELSEIF opr$ = CHR$(13) THEN
  PUT (xmenu, ymenu), etakn, XOR
  fig = 0
  FOR csh = 0 TO 4
   FOR ytm = 1 TO 75000
   NEXT ytm
   IF ymenu = 115 THEN
    PUT (xmenu - fig, ymenu + fig), kn1, XOR
    PUT (xmenu - csh, ymenu + csh), kn1, PSET
   ELSEIF ymenu = 175 THEN
    PUT (xmenu - fig, ymenu + fig), kn2, XOR
    PUT (xmenu - csh, ymenu + csh), kn2, PSET
   ELSEIF ymenu = 235 THEN
    PUT (xmenu - fig, ymenu + fig), kn3, XOR
    PUT (xmenu - csh, ymenu + csh), kn3, PSET
   ELSEIF ymenu = 295 THEN
    PUT (xmenu - fig, ymenu + fig), kn4, XOR
    PUT (xmenu - csh, ymenu + csh), kn4, PSET
   END IF
   fig = csh
  NEXT csh
  GOTO 120
 END IF
END IF
IF ymenu2 > 114 AND ymenu2 < 296 THEN
 IF ymenu2 <> ymenu THEN
  PUT (xmenu, ymenu), etakn, XOR
  ymenu = ymenu2
  PUT (xmenu, ymenu), etakn
 END IF
ELSE
 IF ymenu2 <> ymenu THEN
  IF ymenu2 <= 114 THEN ymenu2 = 295
  IF ymenu2 >= 296 THEN ymenu2 = 115
  PUT (xmenu, ymenu), etakn, XOR
  PUT (xmenu, ymenu2), etakn
  ymenu = ymenu2
 END IF
END IF
LOOP
120 IF ymenu = 115 THEN  'игра
 SOUND 3000, 3
 SOUND 3200, 3
 SOUND 1200, 5
 SLEEP 1
 IF menupokaz = 1 THEN
  menudvij = 0
  lihster = 0
 ELSE
  menudvij = 1
 END IF
 CLS
 menubil = 1
 EXIT SUB
ELSEIF ymenu = 175 THEN  'справка
' menudvij = 0
CLS
'SHARED z3() AS INTEGER
SHARED z1() AS INTEGER
SHARED z4() AS INTEGER
SHARED z6() AS INTEGER
SHARED z7() AS INTEGER
SHARED z5() AS INTEGER
SHARED jizn() AS INTEGER

LOCATE 1, 30
PRINT "СПРАВОЧНАЯ СИСТЕМА"
LOCATE 2, 3
PRINT "Справочная система содержит основные сведения об игре."
LOCATE 3, 1
PRINT "Цель игры: главный герой (его зовут Хряпочка) должен собрать определенное количество монет и дойти до выхода из лабиринта. Главный герой обозначается "
PUT (560, 24), z3
LOCATE 4, 73
PRINT "."
LOCATE 4, 75
PRINT "Стены лабиринта обозначаются так:"
PUT (265, 33), z1
LOCATE 5, 36
PRINT "."
LOCATE 5, 38
PRINT "Передвигаться по лабиринту можно"
LOCATE 5, 72
PRINT "с помощью"
LOCATE 6, 1
PRINT "клавиш вверх, вниз, вправо и влево. Хряпочка имеет несколько жизней. Их количество показано в верхнем правом углу экрана. Пополнять запас жизней можно найдя  и съев символ жизни:"
PUT (145, 56), jizn
LOCATE 8, 21
PRINT ". Один такой символ прибавляет 1 жизнь. Монеты обозначены"
PRINT "так:"
PUT (35, 64), z4
LOCATE 9, 7
PRINT "и"
PUT (58, 64), z7
LOCATE 9, 10
PRINT ",причем монета"
PUT (184, 64), z4
LOCATE 9, 26
PRINT "дает 1 монету, если ее съесть, а монета"
PUT (516, 64), z7
LOCATE 9, 70
PRINT "дает 10"
LOCATE 10, 1
PRINT "монет. Когда вы соберете необходимое количество монет, сверху экрaна появится сообщение об этом, и тогда вы сможете идти к выходу из лабиринта."
LOCATE 11, 65
PRINT "На пути к выходу из лабиринта, обозначенному так:"
PUT (400, 88), z5
LOCATE 12, 53
PRINT ", могут встретиться опасные"
LOCATE 13, 1
PRINT "монстры-"
PUT (65, 94), z6
LOCATE 13, 11
PRINT ", которые отнимают у хряпочки 1 или 5 жизней."
LOCATE 13, 58
PRINT "Монстры могут быть уничтожены с помощью бомб-   , которые необходимо сначала съесть, а затем их можно поставить, нажав клавишу b. Бомбы уничтожают также и решетки-   . Если вы попадете под опускающуюся решетку, у вас отнимется 1 жизнь."
SHARED bomb() AS INTEGER
PUT (361, 103), bomb, PSET
LOCATE 17, 1
PRINT  _
"При постановке бомбы будьте осторожны- не забывайте отходить от нее на несколько шагов, иначе у вас отнимется 1 жизнь. В некоторых местах лабиринта встречается лед-   , который после того, как вы по нему прошли, превращается в воду. Если вы окажитесь в воде , у вас отнимется 1 жизнь, если вы до этого не возьмете лодку-  , позволяющую вам находиться в воде. Иногда путь " _

SHARED res1() AS INTEGER
PUT (29, 119), res1, PSET
SHARED led() AS INTEGER
PUT (39, 143), led, PSET
SHARED lodka() AS INTEGER
LOCATE 21, 52
PRINT "преграждается дверью-    ."
SHARED dver() AS INTEGER
PUT (590, 160), dver, PSET
LOCATE 22, 1
PRINT "Вы не можете пройти через дверь, не отперев ее ключом-   . Также путь может быть загорожен деревянной стеной-   , которую можно взорвать бомбой. "
PUT (7, 159), lodka, PSET
SHARED klut() AS INTEGER
PUT (440, 168), klut, PSET
SHARED stena() AS INTEGER
PUT (240, 176), stena, PSET
LOCATE 24, 1
PRINT "Чтобы помочь вам ориентироваться в лабиринте, в нижнем правом углу экрана находится компас, указывающий своей желтой стрелкой направление на выход."
LOCATE 29, 3
PRINT "ОСНОВНЫЕ КЛАВИШИ:"
LOCATE 31, 2
PRINT "стрелки -перемещение Хряпочки"
LOCATE 32, 2
PRINT "b       -поставить бомбу"
LOCATE 33, 2
PRINT "ПРОБЕЛ  -пауза"
LOCATE 34, 2
PRINT "m       -поставить мину"
LOCATE 35, 2
PRINT "r       -разминировать мину"
LOCATE 37, 2
PRINT "Желаем вам интересной игры!"
LOCATE 40, 1
PRINT " Разработка движка, программирование алгоритмов, дизайн уровней, создание оригинальных моделей, издательство в России - компания BRINCHUK PETROLEUM в лице г-на Бринчука М.М."
LOCATE 57, 2
PRINT "All rights reserved."
LOCATE 58, 2
PRINT "(c) BRINCHUK PETROLEUM, 2002"
DIM opros AS STRING
DO
 opros = INKEY$
 IF opros = CHR$(27) THEN EXIT DO
LOOP
GOTO 130
' DO
'  opros2$ = INKEY$
'  IF opros2$ = CHR$(27) THEN
'   GOTO 130
'  END IF
' LOOP
ELSEIF ymenu = 235 THEN 'установки
 CLS
 LOCATE 1, 10
 PRINT "Вы, наверное, удивляетесь, куда попали? А вы попали в УСТАНОВКИ!"
 LINE (0, 15)-(639, 15), 13
 LOCATE 35, 3
 PRINT "Для выхода из установок наберите 123"
 LOCATE 4, 3
 INPUT "Введите код ", kodik1
 LOCATE 5, 3
 INPUT "Введите код N2 ", kodik2
 LOCATE 6, 70
 PRINT "(0-да, 1-нет)"
 LOCATE 6, 3
 INPUT "Вы желаете, чтобы на экране было показано расстояние до выхода? ", jel
 IF jel = 123 OR kodik1 = 123 THEN
  GOTO 130
 END IF
 LOCATE 8, 50
 PRINT "(0-очень быстро, 1-быстро, 2-средне, 3-медленно)"
 LOCATE 8, 3
 INPUT "Скорость передвижения монстров: ", fun1
 IF fun1 = 0 THEN
  mondvij = 10
 ELSEIF fun1 = 1 THEN
  mondvij = 20
 ELSEIF fun1 = 2 THEN
  mondvij = 75
 ELSEIF fun1 = 3 THEN
  mondvij = 150
 ELSEIF fun1 = 123 THEN
  GOTO 130
 END IF
 LOCATE 20, 20
 PRINT "Спасибо за использование УСТАНОВОК"
 SLEEP 2
 GOTO 130
ELSEIF ymenu = 295 THEN
 END
END IF
END SUB

SUB perevod
'z1-стена               {*}
'z2-пустота             {.}
'z3-сам главный герой   {z}
'z4-монета              {m}
'z5-выход из лабиринта  {e}
'z6-монстр              {M}
'jizn-жизнь             {j}
'dver-дверь             {d}
'klut-ключ              {k}
'krov-кровища           {K}
'z7-супермонета         {s}
'bomb-бомба             {b}
'stena-стена            {S}
'supermon-супермонстр   {G}
'voda1-вода             {w}
'led-лед                {l}
'lodka-.....            {L}
'smertmon-смерть монам  {D}
'sar-шар                {h}
'mina-...               {i}
'minst-мина активная    {N}
END SUB

SUB putsprites STATIC
CLS
SHARED per1() AS INTEGER
SHARED per2() AS INTEGER
SHARED xexit AS INTEGER, yexit AS INTEGER
SHARED mina() AS INTEGER
SHARED minst() AS INTEGER
SHARED krov() AS INTEGER
SHARED z1() AS INTEGER
SHARED z2() AS INTEGER
SHARED z3() AS INTEGER
SHARED z4() AS INTEGER
SHARED z5() AS INTEGER
SHARED z6() AS INTEGER
SHARED z7() AS INTEGER
SHARED jizn() AS INTEGER
SHARED dver() AS INTEGER
SHARED klut() AS INTEGER
SHARED bomb() AS INTEGER
SHARED stena() AS INTEGER
SHARED res1() AS INTEGER
SHARED voda1() AS INTEGER
SHARED led() AS INTEGER
SHARED lodka() AS INTEGER
SHARED smertmon() AS INTEGER
SHARED b() AS STRING * 1
'SHARED sar() AS INTEGER
SHARED sarnum AS INTEGER
SHARED lednum AS INTEGER
SHARED watertrig() AS INTEGER
SHARED coordx AS INTEGER, coordy AS INTEGER
SHARED xo() AS INTEGER
SHARED n AS INTEGER, v AS INTEGER
SHARED allmon AS INTEGER
SHARED kolvomon AS INTEGER
SHARED resnum AS INTEGER
SHARED waternum AS INTEGER
watled = 0
allmon = 0
FOR i = 1 TO v
 FOR u = 1 TO n
 IF b(i, u) = "z" THEN
  coordx = u
  coordy = i
 END IF
 NEXT u
NEXT i
istinx = 310
istiny = 230
FOR i = 1 TO v
 FOR u = 1 TO n
 xist = 310 - (coordx - u) * 10
 yist = 230 - (coordy - i) * 10
 IF b(i, u) = "m" THEN
  allmon = allmon + 1
 END IF
 IF b(i, u) = "r" THEN
  resnum = resnum + 1
 END IF
 IF b(i, u) = "w" THEN
  waternum = waternum + 1
 END IF
 IF b(i, u) = "l" THEN
  lednum = lednum + 1
 END IF
' IF b(i, u) = "h" THEN
'  sarnum = sarnum + 1
' END IF
IF xist >= 0 AND xist <= 619 AND yist >= 10 AND yist <= 469 THEN
 IF b(i, u) = "*" THEN
  PUT (xist, yist), z1, PSET
 ELSEIF b(i, u) = "." THEN
  PUT (xist, yist), z2, PSET
 ELSEIF b(i, u) = "z" THEN
  PUT (xist, yist), z3, PSET
 ELSEIF b(i, u) = "m" THEN
  PUT (xist, yist), z4, PSET
 ELSEIF b(i, u) = "e" THEN
  PUT (xist, yist), z5, PSET
  xexit = xist: yexit = yist
 ELSEIF b(i, u) = "M" THEN
  PUT (xist, yist), z6, PSET
 ELSEIF b(i, u) = "j" THEN
  PUT (xist, yist), jizn, PSET
 ELSEIF b(i, u) = "d" THEN
  PUT (xist, yist), dver, PSET
 ELSEIF b(i, u) = "k" THEN
  PUT (xist, yist), klut, PSET
 ELSEIF b(i, u) = "s" THEN
  PUT (xist, yist), z7, PSET
 ELSEIF b(i, u) = "b" THEN
  PUT (xist, yist), bomb, PSET
 ELSEIF b(i, u) = "S" THEN
  PUT (xist, yist), stena, PSET
 ELSEIF b(i, u) = "w" THEN
  PUT (xist, yist), voda1, PSET
 ELSEIF b(i, u) = "l" THEN
  PUT (xist, yist), led, PSET
 ELSEIF b(i, u) = "L" THEN
  PUT (xist, yist), lodka, PSET
 ELSEIF b(i, u) = "D" THEN
  PUT (xist, yist), smertmon, PSET
 ELSEIF b(i, u) = "i" THEN
  PUT (xist, yist), mina, PSET
 ELSEIF b(i, u) = "N" THEN
  PUT (xist, yist), minst, PSET
 ELSEIF b(i, u) = "K" THEN
  PUT (xist, yist), krov, PSET
' ELSEIF b(i, u) = "h" THEN
'  PUT (xist, yist), sar, PSET
 END IF
END IF
NEXT u
NEXT i
LINE (0, 0)-(0, 479), 11
LINE (0, 479)-(639, 479), 11
LINE (639, 479)-(639, 0), 11
LINE (639, 0)-(0, 0), 11
LINE (620, 200)-(639, 300), 11, B
LINE (620, 300)-(639, 400), 11, B
LINE (620, 100)-(639, 200), 11, B
LINE (620, 0)-(639, 100), 11, B
PUT (200, 460), per1, PSET
PUT (215, 460), per1, PSET

END SUB

