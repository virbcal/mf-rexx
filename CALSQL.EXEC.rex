/*
 * Module  : CALSQL
 * Author  : Virgilio B Calimlim - 2015-01-23
 *
 * Function: Execute SQL statements and save output to a file.
 *
 * Applictn: Generic
 *
 * Platform: VM/CMS, MVS
 *
 * Command Syntax:
 *
 *   CALSQL   <positional-parameters> <{> <keyword-paramaters>
 *
 *   CALSQL   - module name
 *   ?        - (pos1) show program documentation
 *   -DOC     - (pos1) show program documentation
 *   -VER     - (pos1) show program version
 *   -NOH     - (pos1) no headers and tail report details
 *   <{>      -        options separator
 *   <kwd>    - (kwd)  keyword with implicit value
 *   <kwd=val>- (kwd)  keyword with expressed value
 *
 *   Keywords: Can be abbreviated to the first 2-4 characters.
 *
 *   TRace    - print trace messages
 *   NTRace   - do not print trace messages
 *   SEnd     - send report to recipient address
 *   NSEnd    - do not send report to recipient address
 *   BRowse   - browse report file at end of process
 *   NBRowse  - do not browse report file at end of process
 *   DBEnvir= - db environment
 *   IFile=   - input file containing the input SQL statements
 *   OFile=   - output report file to contain the query results
 *   MFile=   - output messages file
 *   ADdress= - recipient address (format: userid@nodeid)
 *   HITslim= - hits limit; number of rows to be reported
 *              '*' means all
 *   FULcname - full column name
 *   MCLen=   - max column length
 *              def: 24
 *              '*' length specified at syscolumns
 *
 * Revisions:
 * v.r.m Date       Author   Description
 * 1.0.0 2015-01-29 authorvb Initial release.
 * 1.0.1 2015-02-08 authorvb Replace options separator ( with {.
 * 1.0.2 2015-02-25 authorvb Optimize COUNT & DISTINCT processing.
 * 1.0.3 2015-02-27 authorvb Correct column alignment;
 *                           Identify column functions.
 * 1.0.4 2015-03-14 authorvb Option to display full column name;
 *                           Option to limit max column length.
 * 1.0.5 2015-04-01 authorvb Include access to VMSERVIC database;
 *                           Log all SQL msgs from non-zero RC except 4.
 * 1.0.6 2015-04-04 authorvb Determine if db is VM or MVS based.
 * 1.0.7 2015-07-28 authorvb Rearrange col pos - seq nam len typ.
 * 1.0.8 2015-08-04 authorvb Correct date/time column length display.
 * 1.0.9 2015-09-11 authorvb Correct full column length display.
 * 1.0.a 2015-09-24 authorvb Accept '.' as fname entry for IFile & OFil.
 * 1.0.b 2016-01-06 authorvb Translate x'00' to x'40' fetched length.
 *
 * End-doc == Do not change or remove this line ==
....+....1....+....2....+....3....+....4....+....5....+....6....+....7.|
 */
 
 Trace Off
 Address COMMAND
 Parse Arg parms
 Parse Var parms prms '{' opts
 Parse Var prms pos1 .
 Parse Upper Var pos1 1 pos1 5 .
 Parse Source . ctyp excn exct excm . env .
 
 df_pgmfid= excn exct excm                  /* program fileid         */
 df_pgmdoc= excn 'DOC A'                    /* program documentation  */
 irc = 0
 
 If pos1='?' | pos1='-DOC' Then Signal SHOW_DOC
 If pos1='-VER'            Then Signal SHOW_VER
 
 log_msg = 'EXEC LOGMSG LOGNAME PVT'
 log_msg 'MSGNO .. SEV S CONS YES MSG Exec Begins.'
 
 Call GET_DF_VALUES
 Call GET_KW_PARMS
 Call SET_RT_VALUES
 Call LOG_RT_SETG
 Call PROCESS_SQL
 
 Signal @exit
 
/**********************************************************************/
/* Process SQL command.                                               */
/**********************************************************************/
PROCESS_SQL:
 If prms\='' Then
   'PIPE Literal' prms'| >' $_ifl '| VAR txt'
 Else
   'PIPE <' $_ifl '| JOIN * | VAR txt'
 Parse Var txt . '.' . qlfy
 Parse Upper Var txt sel 'FROM' tcre '.' tnam .
 If tcre\='' & tnam\='' Then Do
   tcre=Strip(tcre); tnam=Strip(tnam)
   sel=Strip(sel)
   If sel='' Then sel = '*'
   Else Do
     Parse Var sel s sel
     If sel='' Then sel = s
   End
   prms = 'SELECT' sel 'FROM' tcre'.'tnam qlfy
 End
 Else Do
  Parse Upper Var txt tcre '.' tnam .
  If tcre\='' & tnam\='' Then Do
   tcre=Strip(tcre); tnam=Strip(tnam)
   sel = '*'
   prms = 'SELECT * FROM' tcre'.'tnam qlfy
  End
  Else Do
   irc = 8
   mssg='Unable to identify CREATOR and TNAME'
  End
 End
 
 If tcre\='' & tnam\='' Then Do
   $_dbb = 'VM'                                    /* deflt db base   */
   If $_dbe='VMSERVIC'
     Then Do; 'EXEC INIT'; $_dbb='MVS'; End
   Else
   If $_dbe='VPLA'  | $_dbe='VPLB'  | ,
      $_dbe='TESTA' | $_dbe='TESTB'
     Then 'EXEC VPLSETUP SQL'||$_dbe
     Else 'EXEC SYINTSQL' $_dbe
   irc = RC
   If irc=0 Then Do
     sql_online = 1
     $_sel = sel
     Call GET_TABLE_DTLS tcre tnam
     If irc=0 Then
       Call PERFORM_QUERY tcre tnam
   End
 End
 
 Return
 
/**********************************************************************/
/* Get table details.                                                 */
/**********************************************************************/
GET_TABLE_DTLS:
 Arg tcre tnam
 actrtn = 'GET_TABLE_DTLS'
 
 "RXSQL CLOSE SQLSVC"
 "RXSQL PURGE SQLSVC"
 
 /*---------------------------------------*/
 /* Get column name, length and sequence. */
 /*---------------------------------------*/
 
 If irc=0 Then Do
  Select
   When $_dbb='VM' Then Do
    qfy_selec="COLNO,CNAME,LENGTH,COLTYPE FROM SYSTEM.SYSCOLUMNS"
    qfy_where="CREATOR='"tcre"' AND TNAME='"tnam"'"
    End
   When $_dbb='MVS' Then Do
    qfy_selec="COLNO,NAME,LENGTH,COLTYPE FROM SYSIBM.SYSCOLUMNS"
    qfy_where="TBCREATOR='"tcre"' AND TBNAME='"tnam"'"
    End
   Otherwise Do
    irc =16
    mssg='Invalid DB base, RC='irc
    log_msg 'MSGNO 80 CONS YES SEV E MSG' mssg
    End
  End
 End
 
 If irc=0 Then Do
   irc=SQL_DRIVER("PREP SQLSVC" ,
   "SELECT" qfy_selec ,
   "WHERE" qfy_where ,
   "ORDER BY COLNO" ,
   )
   Call TRACE_MSG actrtn 'PREP RC='irc
 End
 
 If irc=0 Then Do
   irc=SQL_DRIVER("OPEN SQLSVC")
   Call TRACE_MSG actrtn 'OPEN RC='irc
 End
 
 If irc=0 Then Do
  Call LOAD_PRS_TYPLEN                             /* presentation len*/
  Drop hdrs.; i=0
  Do While irc=0
   irc=SQL_DRIVER("FETCH SQLSVC COLN,CNAM,LENG,COLT")
   If irc=0 Then Do
    i = i + 1
    If coln='' Then coln = '0'
    If cnam='' Then cnam = '.'
    If leng='' Then leng = '6'
    Else            leng = Translate(leng,'40'x,'00'x)        /* @10b */
    If colt='' Then colt = '.'
    leng = Max(leng,GET_PRS_TYPLEN(colt))
    hdrs.i = Translate(coln cnam leng colt,'40'x,'00'x)
   End
  End
  If irc=4 Then Do
   hdrs.0=i; irc=0
  End
  Call TRACE_MSG actrtn 'FETCH RC='irc 'SQLCODE='sqlcode
 End
 
 "RXSQL CLOSE SQLSVC"
 "RXSQL PURGE SQLSVC"
 
 /*----------------------------------------------------*/
 /* Establish max column lengths for report alignment. */
 /*----------------------------------------------------*/
 
 If irc=0 Then Do
  mxcn=0; mxln=0; mxco=Length(hdrs.0); mxct=0
  Do i=1 to hdrs.0
   Parse Var hdrs.i coln cnam leng colt .
   mxcn = Max(mxcn,Length(cnam))
   mxln = Max(mxln,Length(leng))
   mxct = Max(mxct,Length(colt))
  End
  Do i=1 to hdrs.0
   Parse Var hdrs.i coln cnam leng colt .
   If $_ful=1 Then leng = Max(leng,Length(cnam))   /* full col name   */
   Else
   If $_mcl='*' Then NOP                           /* use syscol len  */
   Else
   If leng>$_mcl Then leng = $_mcl                 /* limit length    */
   hdrs.i = Right(coln,mxco) ,                     /* sav all attribs */
             Left(cnam,mxcn) ,
            Right(leng,mxln) ,
             Left(colt,mxct)
  End
  'EXECIO' hdrs.0 'DISKW' $_mfl '(FINIS STEM HDRS.'
 End
 
 /*---------------------------------------*/
 /* Generate fetch variables and headers. */
 /*---------------------------------------*/
 
 If irc=0 & $_sel='*' Then Do                 /* all columns selected */
  ftc=''; cnm=''; dsh=''; ftc.0=hdrs.0
  Do i=1 to hdrs.0
   ftc = ftc 'ftc.'i','                            /* build fetch var */
   Parse Var hdrs.i . cnam leng colt .
   If Pos('INT',colt)>0                            /* is numeric?     */
    Then xxx = Right(cnam,leng)                    /*   Y, right just */
    Else xxx =  Left(cnam,leng)                    /*   N, left just  */
   cnm = cnm xxx                                   /* cname header    */
   dsh = dsh Copies('-',leng)                      /* dash header     */
  End
  If ftc\='' Then ftc = Left(ftc,Length(ftc)-1)    /* remove last ',' */
  Call TRACE_MSG actrtn 'ftc='ftc
 End
 
 If irc=0 & $_sel\='*' Then Do           /* specific columns selected */
  txt=$_sel
  i=0; ftc=''; cnm=''; dsh=''; Drop ftc.
  Drop selh.                                      /* selection header */
  Do While txt\=''
   Parse Var txt tmp ',' txt
   If tmp\='' Then Do
    i=i+1; ftc = ftc 'ftc.'i','                    /* build fetch var */
    tmp = Strip(tmp)
    If Lastpos(')',tmp)=Length(tmp) Then Do        /* function call   */
     cnam= 'FUNCTION'; len = 21; typ = 'INT'       /* set col attribs */
     cnm = cnm Left(tmp,len)                       /* cname header    */
     dsh = dsh Copies('-',len)                     /* dash header     */
     selh.i = i cnam len typ                       /* sav sel attribs */
    End
    Else Do
     If Substr(tmp,1,8)='DISTINCT' Then Do         /* distinct keywd  */
       Parse Var tmp . tmp
       Parse Var tmp tmp ')' .
     End
     Do j=1 to hdrs.0
      Parse Var hdrs.j . cnam leng colt .
      If tmp=cnam Then Do
       If Pos('INT',colt)>0                        /* is numeric?     */
        Then xxx = Right(cnam,leng)                /*   Y, right just */
        Else xxx =  Left(cnam,leng)                /*   N, left just  */
       cnm = cnm xxx                               /* cname header    */
       dsh = dsh Copies('-',leng)                  /* dash header     */
       selh.i = i tmp leng colt                    /* sav sel attribs */
       Leave j
      End
     End /* j     */
    End /* else  */
   End  /* tmp   */
  End   /* while */
  ftc.0=i; selh.0=i
  If ftc\='' Then ftc = Left(ftc,Length(ftc)-1)    /* remove last ',' */
  Call TRACE_MSG actrtn 'ftc='ftc
 End    /* irc   */
 
 Return irc
 
/**********************************************************************/
/* Perform query.                                                     */
/**********************************************************************/
PERFORM_QUERY:
 Arg tcre tnam
 actrtn = 'PERFORM_QUERY'
 
 "RXSQL CLOSE SQLSVC"
 "RXSQL PURGE SQLSVC"
 
 /*--------------------------*/
 /* Process query statement. */
 /*--------------------------*/
 
 irc=SQL_DRIVER("PREP SQLSVC" prms)
 Call TRACE_MSG actrtn 'PREP RC='irc
 
 If irc=0 Then Do
   irc=SQL_DRIVER("OPEN SQLSVC")
   Call TRACE_MSG actrtn 'OPEN RC='irc
 End
 
 If irc=0 Then Do                          /* build & align report    */
  Drop dtls.; i=0
  Do While irc=0
   irc=SQL_DRIVER("FETCH SQLSVC" ftc)
   If irc=0 Then Do
    i=i+1; dtls.i = ''
    Do j=1 to ftc.0                        /* get column attributes   */
     If $_sel='*'
      Then Parse Var hdrs.j . . len typ .  /* ...get attr fm all cols */
      Else Parse Var selh.j . . len typ .  /* ...get attr fm sel cols */
     If Pos('INT',typ)>0
      Then txt = Right(ftc.j,len)
      Else txt =  Left(ftc.j,len)
     dtls.i = dtls.i txt                   /* save details            */
    End
   End
  End
  If irc=4 Then Do
   dtls.0=i; irc=0
  End
  Call TRACE_MSG actrtn 'FETCH RC='irc 'SQLCODE='sqlcode
 End
 
 "RXSQL CLOSE SQLSVC"
 "RXSQL PURGE SQLSVC"
 
 /*------------------*/
 /* Generate report. */
 /*------------------*/
 
 If irc=0 Then Do
   'PIPE Literal ' prms'| >' $_ofl
   'EXECIO 1 DISKW'        $_ofl '(STR'
   'EXECIO 1 DISKW'        $_ofl '(STR' cnm
   'EXECIO 1 DISKW'        $_ofl '(STR' dsh
   'EXECIO' dtls.0 'DISKW' $_ofl '(STEM DTLS.'
   'EXECIO 1 DISKW'        $_ofl '(FINIS STR' ,
     ' * End of Result ***' dtls.0 'Rows Displayed ***'
 End
 
 Return irc
 
/***********************************************************************
 *                                                                     *
 * SQL driver.                                                         *
 *                                                                     *
 *       run all sql through this driver                               *
 *                                                                     *
 *       rc = 0   sucessful                                            *
 *       rc = 2   duplicate data                                       *
 *       rc = 4   end of data                                          *
 *       rc = 8   sql is offline                                       *
 *       rc = 16  error has occured                                    *
 *       -execute passed sql command                                   *
 *       -log any sql error, ^(0 | 100 | -803)                         *
 *                                                                     *
 **********************************************************************/
SQL_DRIVER:
 Parse Arg sql_cmd
 'RXSQL' sql_cmd
 sql_rc = RC
 Select
  When sql_rc=0 | (sql_rc=4 & sqlcode=100)    /* ok or end-of-data    */
   Then irc = sql_rc
  Otherwise Do                                /* sql statement failed */
   If sqlcode=-803                            /* duplicate data       */
    Then Do
     irc = 2
     mssg= 'SQL Duplicate data, RC:'sql_rc
    End
   Else
   If sqlcode=-933 | sqlcode=-940 | sqlcode=-948 /* dbid unavailable  */
    Then Do
     sql_online = 0
     irc = 8
     mssg= 'SQL DB unavailable, RC:'sql_rc
    End
   Else Do                                    /* other sql error     */
     irc = 16
     mssg= 'SQL Transaction failed, RC:'sql_rc
   End
   log_msg 'MSGNO 80 CONS YES SEV E MSG >'mssg
   log_msg 'MSGNO 80 CONS YES SEV E MSG >RC      :'sql_rc
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLCODE :'sqlcode
   log_msg 'MSGNO 80 CONS YES SEV E MSG >RXSQLMSG:'rxsqlmsg
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLNAMES:'sqlnames
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLSTATE:'sqlstate
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRM :'sqlerrm
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRP :'sqlerrp
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLWARN :'sqlwarn
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.1:'sqlerrd.1
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.2:'sqlerrd.2
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.3:'sqlerrd.3
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.4:'sqlerrd.4
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.5:'sqlerrd.5
   log_msg 'MSGNO 80 CONS YES SEV E MSG >SQLERRD.6:'sqlerrd.6
   Call LOG_MSSG '>RC      :'sql_rc
   Call LOG_MSSG '>SQLCODE :'sqlcode
   Call LOG_MSSG '>RXSQLMSG:'rxsqlmsg
   Call LOG_MSSG '>SQLNAMES:'sqlnames
   Call LOG_MSSG '>SQLSTATE:'sqlstate
   Call LOG_MSSG '>SQLERRM :'sqlerrm
   Call LOG_MSSG '>SQLERRP :'sqlerrp
   Call LOG_MSSG '>SQLWARN :'sqlwarn
   Call LOG_MSSG '>SQLERRD.1:'sqlerrd.1
   Call LOG_MSSG '>SQLERRD.2:'sqlerrd.2
   Call LOG_MSSG '>SQLERRD.3:'sqlerrd.3
   Call LOG_MSSG '>SQLERRD.4:'sqlerrd.4
   Call LOG_MSSG '>SQLERRD.5:'sqlerrd.5
   Call LOG_MSSG '>SQLERRD.6:'sqlerrd.6
  End /* otherwise */
 End  /* select    */
 Return irc
 
/**********************************************************************/
/* Load default presentation column type-length table.                */
/**********************************************************************/
LOAD_PRS_TYPLEN: Procedure Expose tltab.
 ttab='DATE(10) TIME(8) TIMESTMP(14) TIMESTZ(16)'
 tmp =ttab
 Drop tltab.
 tltab.0=Words(ttab)
 Do i=1 to tltab.0
   Parse Var tmp typ'('len')' tmp
   tltab.i   = Strip(typ)
   tltab.i.l = len
 End
 Return
 
/**********************************************************************/
/* Get default presentation length by column type.                    */
/**********************************************************************/
GET_PRS_TYPLEN: Procedure Expose tltab.
 Parse Arg typ .
 len = 0
 Do i=1 to tltab.0
   If typ=tltab.i Then len = tltab.i.l
 End
 Return len
 
/**********************************************************************/
/* Log trace message.                                                 */
/**********************************************************************/
TRACE_MSG:
 If $_tra=1 Then Do
   Parse Arg tmsg
   Call LOG_MSSG tmsg
   Say tmsg
 End
 Return
 
/**********************************************************************/
/* Get default values.                                                */
/**********************************************************************/
GET_DF_VALUES:
 /* init default values                                               */
 df_dbe = 'VM'                              /* db environment         */
 df_dbb = 'VM'                              /* db environment base    */
 df_ifl = excn 'SQLIN A'                    /* input parms file       */
 df_ofl = excn 'SQLOUT A'                   /* output results file    */
 df_mfl = excn 'MESSAGES A'                 /* output messages file   */
 df_add = 'authorvb@ph.xxx.com'             /* recipient addr         */
 df_ttl = 'SQL Query Results'               /* report title           */
 df_tra = 0                                 /* dont print trace msgs  */
 df_snd = 0                                 /* dont send report       */
 df_bro = 1                                 /* browse report          */
 df_hit = '*'                               /* max rows               */
 df_mcl = 24                                /* max col lenght         */
 df_ful = 0                                 /* not full col name      */
 /* init options values                                               */
 op_dbe = ''                                /* db environment         */
 op_ifl = ''                                /* input parms file       */
 op_ofl = ''                                /* output results file    */
 op_mfl = ''                                /* output messages file   */
 op_add = ''                                /* recipient addr         */
 op_tra = ''                                /* print trace messages   */
 op_snd = ''                                /* send report            */
 op_bro = ''                                /* browse report          */
 op_hit = ''                                /* max rows               */
 op_mcl = ''                                /* max col lenght         */
 op_ful = ''                                /* full col name          */
 Return
 
/**********************************************************************/
/* Get keyword parameters.                                            */
/**********************************************************************/
GET_KW_PARMS:
 Do While opts\='' & irc=0
   Parse Var opts prm opts
   If prm\='' Then Do
     Parse Var prm arg1 '=' val
     Upper arg1
     Parse Upper Var val uval
     If val='' Then Do
       Select
         When Abbrev('TRACE',arg1,2) Then   /* print trace messages   */
           op_tra = 1
         When Abbrev('NTRACE',arg1,3) Then  /* dont print trace msgs  */
           op_tra = 0
         When Abbrev('SEND',arg1,2) Then    /* send report file       */
           op_snd = 1
         When Abbrev('NSEND',arg1,3) Then   /* dont send report       */
           op_snd = 0
         When Abbrev('BROWSE',arg1,2) Then  /* browse report file     */
           op_bro = 1
         When Abbrev('NBROWSE',arg1,3) Then /* dont browse report     */
           op_bro = 0
         When Abbrev('VERSION',arg1,3) Then /* show version           */
           p_ver = 1
         When Abbrev('DOCUMENT',arg1,3) Then/* show documentation     */
           op_doc = 1
         When Abbrev('FULCNAME',arg1,3) Then/* full column name       */
           op_ful = 1
         Otherwise Do
           irc = 8
           mssg='Invalid keyword parameter:' arg1
         End
       End
     End
     Else Do
       Select
         When Abbrev('DBENVIR',arg1,3) Then /* db environment         */
           op_dbe = uval
         When Abbrev('IFILE',arg1,2) Then   /* input file             */
           op_ifl = PARS_FILE(arg1 val opts)
         When Abbrev('OFILE',arg1,2) Then   /* report file            */
           op_ofl = PARS_FILE(arg1 val opts)
         When Abbrev('MFILE',arg1,2) Then   /* messages file          */
           op_mfl = PARS_FILE(arg1 val opts)
         When Abbrev('ADDRESS',arg1,2) Then /* recipient address      */
           op_add = val
         When Abbrev('HITSLIM',arg1,3) Then /* hits limit             */
           op_hit = VAL_WNUM(arg1 val)
         When Abbrev('MCLEN',arg1,3) Then   /* max column length      */
           op_mcl = VAL_WNUM(arg1 val)
         Otherwise Do
           irc = 8
           mssg='Invalid keyword parameter:' arg1
         End
       End
     End
   End
 End
 If irc\=0 Then Signal @exit
 Return
 
/**********************************************************************/
/* Get fileid.                                                        */
/**********************************************************************/
PARS_FILE: Procedure Expose opts irc mssg excn
 Parse Arg kwd opts
 Parse Var opts fn ft fm more
 If fn='.' Then fn = excn
 lfn = Length(fn)
 lft = Length(ft)
 lfm = Length(fm)
 If lfn>0 & lfn<9 & ,
    lft>0 & lft<9 & ,
    lfm>0 & lfm<3 Then Do
    uval = fn ft fm; Upper uval
    opts = more
    Return uval
 End
 irc = 8
 mssg='Invalid parameter value:' kwd'='opts
 Return mssg
 
/**********************************************************************/
/* Validate whole number.                                             */
/**********************************************************************/
VAL_WNUM: Procedure Expose irc mssg
 Parse Arg kwd val
 If val='*' |,
    Datatype(val,'W')=1 Then Return val
 irc = 8
 mssg='Invalid parameter value:' kwd'='val
 Return mssg
 
/**********************************************************************/
/* Set runtime values.                                                */
/**********************************************************************/
SET_RT_VALUES:
 If op_dbe\='' Then $_dbe = op_dbe          /* db environment         */
 Else               $_dbe = df_dbe
 If op_ifl\='' Then $_ifl = op_ifl          /* input parms file       */
 Else               $_ifl = df_ifl
 If op_ofl\='' Then $_ofl = op_ofl          /* output results file    */
 Else               $_ofl = df_ofl
 If op_mfl\='' Then $_mfl = op_mfl          /* output messages file   */
 Else               $_mfl = df_mfl
 If op_add\='' Then $_add = op_add          /* recipient addr         */
 Else               $_add = df_add
 If op_tra\='' Then $_tra = op_tra          /* print trace messages   */
 Else               $_tra = df_tra
 If op_snd\='' Then $_snd = op_snd          /* send report            */
 Else               $_snd = df_snd
 If op_bro\='' Then $_bro = op_bro          /* browse report          */
 Else               $_bro = df_bro
 If op_hit\='' Then $_hit = op_hit          /* max rows               */
 Else               $_hit = df_hit
 If op_ful\='' Then $_ful = op_ful          /* full column name       */
 Else               $_ful = df_ful
 If op_mcl\='' Then $_mcl = op_mcl          /* max column lenght      */
 Else               $_mcl = df_mcl
 Parse Var $_add $_uid '@' $_nid
 Parse Var Reverse($_nid) tmp '.'
 ltmp = Length(tmp)
 If ltmp>0 & ltmp<4 Then $_ttl = df_ttl     /* report title           */
 Else                    $_ttl = ''
 verd = GET_VER()
 Return
 
/**********************************************************************/
/* Log runtime settings.                                              */
/**********************************************************************/
LOG_RT_SETG:
 'PIPE Literal | >' $_mfl
 Call LOG_MSSG '*===' excn '- Runtime Configuration ==='
 Call LOG_MSSG ' ModuleName ='excn verd
 Call LOG_MSSG ' CallType   ='ctyp
 Call LOG_MSSG ' Environment='env
 Call LOG_MSSG ' PARMS ='parms
 Call LOG_MSSG ' DBENV ='$_dbe
 Call LOG_MSSG ' IFILE ='$_ifl
 Call LOG_MSSG ' OFILE ='$_ofl
 Call LOG_MSSG ' MFILE ='$_mfl
 Call LOG_MSSG ' ADDRE ='$_add
 Call LOG_MSSG ' TITLE ='$_ttl
 Call LOG_MSSG ' TRACE ='$_tra
 Call LOG_MSSG ' SEND  ='$_snd
 Call LOG_MSSG ' BROWSE='$_bro
 Call LOG_MSSG ' FULCNA='$_ful
 Call LOG_MSSG ' MCLEN ='$_mcl
 Call LOG_MSSG 'Messages:'
 Return
 
/**********************************************************************/
/* Log message in messages file.                                      */
/**********************************************************************/
LOG_MSSG: Procedure Expose $_mfl
 Parse Arg mssg
 'EXECIO 1 DISKW' $_mfl '(FINIS STR' mssg
 Return
 
/**********************************************************************/
/* Send file.                                                         */
/**********************************************************************/
SEND_FILE:
 Arg sndfle
 If $_ttl\='' Then tmp = "(MIME ASCII-ATTACH SUBJ '"$_ttl"'"
 Else              tmp = ''
 'SENDFILE' sndfle 'TO' $_uid 'AT' $_nid tmp
 Return
 
/**********************************************************************/
/* Show program version.                                              */
/**********************************************************************/
SHOW_VER:
 Say excn GET_VER()
 Exit
 
/**********************************************************************/
/* Get program version.                                               */
/**********************************************************************/
GET_VER:
 Drop verdte.
 'PIPE <' df_pgmfid ,
 '| FRLABel  * v.r.m',
 '| TOLABel  * End-doc',
 '| SPECs 4;19 1',
 '| STRIP',
 '| Locate',
 '| Stem verdte.'
 i = verdte.0
 Return verdte.i
 
/**********************************************************************/
/* Show program documentation.                                        */
/**********************************************************************/
SHOW_DOC:
 'PIPE <' df_pgmfid ,
 '| TOLABel  * End-doc',
 '| >' df_pgmdoc
 If $_snd=1 Then Call SEND_FILE df_pgmdoc
 'BROWSE' df_pgmdoc
 Exit
 
/**********************************************************************/
/* Common exit routine.                                               */
/**********************************************************************/
@exit:
 log_msg 'MSGNO .. SEV T CONS YES MSG Exec Ends, Return Code:' irc
 If irc=0 Then mssg=irc excn 'I: Process completed.'
 Else          mssg=irc excn 'E:' mssg
 Call LOG_MSSG mssg
 If irc=0 Then Do
   If $_snd=1 Then Call SEND_FILE $_ofl
   If $_bro=1 Then Do; 'PIPE CMS BROWSE' $_ofl
                       'PIPE CMS BROWSE' $_mfl; End
 End
 Say mssg
 Exit irc
