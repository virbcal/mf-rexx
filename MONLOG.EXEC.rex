/* MONLOG - Monitor log files and search for error messages
 * Vir Calimlim - 2011-08-23
 *
 * Applictn: Service Acquisition Systems (PTFC and VPL)
 *
 * Revsions:
 * ver.rel Date       Author   Description
 *   1.00  2011-08-23 authorvb Initial release
 *   1.01a 2011-08-30 authorvb MVSRTN added
 *   1.01b 2011-09-03 authorvb WRKSTRTN added
 *   1.01c 2011-09-07 authorvb VMRTN added
 *   1.01d 2011-09-10 authorvb VPLRTN added
 *   1.01e 2011-09-22 authorvb SRCHLOG2 & SRCHLOG3 added;
 *                             VPLWRKnn search added;
 *                             VPLRTN removed, replaced by SRCHLOG3.
 *   1.01f 2011-09-27 authorvb SRCHRTN3, VSERTN, VSERTN2 added;
 *                             SRCHLOG3 now a single pipe routine.
 *   1.01g 2011-09-30 authorvb Reorg SRCHLOG1/2/3, SRCHRTN1/2/3.
 *   1.02a 2011-12-05 authorvb Check RDR files for VPL & MVS;
 *                             STATE replaces LISTFILE;
 *                             APPEND replaces FANIN.
 *   1.02b 2012-02-24 authorvb Check ID files for VPL using external
 *                             call to CALQVPL.
 *   1.02c 2012-05-05 authorvb Remove existence check of FTP_AL EXEC;
 *                             External call to CALGWKNO.
 *   1.02d 2012-05-31 authorvb Correct search strings for VMSERVIC.
 *   1.2.5 2012-10-21 authorvb Check disk usage for each appl group
 *                             using external call to CALGDU;
 *   1.3   2013-01-17 authorvb Revise RDR check to detect process that stopped;
 *   1.3a  2013-01-22 authorvb Add monitoring of OVERFLOW SUBSET in SMASAP;
 *   1.3b  2013-01-31 authorvb Adjust num-digits of the Format output;
 *   1.3c  2013-02-08 authorvb Adjust warning levels;
 *   1.3d  2013-02-28 authorvb Timecheck for MVSERR PDO file check is >= 7:15am;
 *   1.3e  2013-04-04 authorvb Include MMVSCLOS checking;
 *                             Add show-ver and show-doc routines;
 *   1.3f  2013-04-19 authorvb Strip trailing blanks;
 *   1.3g  2013-11-15 authorvb Separate alert for VPL sqlcode:-712;
 *   1.3h  2013-12-12 authorvb Update parms for CALGDU;
 *   1.3i  2013-12-18 authorvb Add monlog for VMTRACK;
 *                             Add runtime;
 *   1.3j  2014-02-19 authorvb Increase Overflow Subset threshold to 98%;
 *   1.3k  2014-02-27 authorvb Increase Overflow Subset threshold to 99%;
 *                             don't send warning until <=10 recs left;
 *   1.3l  2015-11-09 authorvb Add monlog for VMCONS and VSECONS;
 *
 * End-doc == Do not change or remove this line ==
 */
'SET MSG OFF';'SET IMSG OFF';'SET EMSG OFF';'SET WNG OFF';'SET SM OFF'
 
/*..+....1....+....2....+....3....+....4....+....5....+....6....+....7....+...*/
Parse Source . . sysn syst sysm .
Parse Arg prms
If Substr(prms,1,1)='?' Then Signal SHOW_DOC
If Translate(Substr(prms,1,4))='-VER' Then Signal SHOW_VER
 
Parse Var prms sdte emon emvs evm eaix .
dates = DATE('S')
if sdte = '' | sdte = '.' then sdte = SUBSTR(DATE('U'),1,6) ||, /* mm/dd/ccyy */
                                      SUBSTR(dates,1,4)
if emon = '' | emon = '.' then emon = 'authorvb@ph.xxx.com'     /* mon email  */
if emvs = '' | emvs = '.' then emvs = 'authorvb@ph.xxx.com'     /* mvs email  */
if evm  = '' | evm  = '.' then evm  = 'authorvb@ph.xxx.com'     /* vm  email  */
if eaix = '' | eaix = '.' then eaix = 'authorvb@ph.xxx.com'     /* aix email  */
evse = evm ; esmr = evm ; eses = evm                            /* jim's      */
eas4 = eaix; ecat = eaix; ewrk = eaix; evpl = eaix;             /* tim's      */
 
wmod = 'A'                                                      /* write mode */
ydte = SUBSTR(sdte,9,2)                                         /* yy         */
ofle = sysn 'ML'        ||,                                     /* MLyymmdd   */
       ydte             ||,
       SUBSTR(sdte,1,2) ||,
       SUBSTR(sdte,4,2)   ,
       wmod
oflh = sysn 'ML$TMPH' wmod                                      /* hdrfile    */
oflw = sysn 'ML$TMPW' wmod                                      /* wrkfile    */
oflm = sysn 'ML$TMPM' wmod                                      /* mrgfile    */
oflt = sysn 'ML$TMPT' wmod                                      /* tmpfile    */
oflx = sysn 'ML$TMPX' wmod                                      /* extfile    */
udte = SUBSTR(sdte,1,6) || ydte                                 /* mm/dd/yy   */
wday = DATE('W',udte,'U')                                       /* weekday    */
 
rttl = sysn sdte                                          /* run title */
rtme = Time()
CALL prntrep
'ACCESSM0 ON'
 
'GETFMADR F100'
Parse Pull . mode vdev zmode .
If zmode = mode then mode = 0
lkdv = '191'
 
/* Uncomment to perform test
Signal DO_TEST               */
 
/* Move to test code
DO_TEST:
Signal DO_SENDF              */
 
/* MVS - KERR */
If wday = 'Thursday' then CALL mvsrtn
infl = 'MVS      LOGPRINT'
lkid = 'MMVSCLOS'
CALL srchlog
 
/* VM - KVM */
infl = 'PVP      LOGPRINT'
lkid = 'M76MSTAT'
CALL srchlog
lkid = 'PH24PTFS'
CALL srchlog
lkid = 'VMSERVIC'
CALL srchlog
CALL vmrtn
lkid = 'VMTRACK'
CALL srchlog
lkid = 'VMCONS'
infl = 'VMCONS   LOGPRINT'
CALL srchlog
 
/* VSE */
CALL vsertn
lkid = 'PTFNET'
infl = 'VSE      LOGPRINT'
CALL srchlog
lkid = 'VSECONS'
infl = 'VSECONS  LOGPRINT'
CALL srchlog
 
/* AIX */
lkid = 'AIXRCV'
infl = 'AIX      LOGPRINT'
CALL srchlog
infl = 'WORKSTN  LOGPRINT'
CALL srchlog
 
/* CATIA */
lkid = 'PTFRCV'
infl = 'CATIA    LOGPRINT'
CALL srchlog
infl = 'WORKSTN  LOGPRINT'
CALL srchlog
 
/* AS400 */
lkid = 'XPFRCV'
infl = 'OLYMPIC  LOGPRINT'
CALL srchrtn1 'COPYFILE', eas4
infl = 'WORKSTN  LOGPRINT'
CALL srchlog
 
/* WORKSTATION */
lkid = 'WORKSRCV'
infl = 'WORKSTN  LOGPRINT'
CALL srchlog
CALL wrkstrtn
lkdv = '191'
lkid = 'TOOLIBTS'
infl = 'TOOLIBTS LOGPRINT'
CALL srchlog
 
/* SMRG */
CALL smrgrtn
/*
lkid = 'SMRG01'
infl = 'RSUUPDAT LOGPRINT'
CALL srchlog
lkid = 'SMRG02'
lkid = 'SMRG03'
lkid = 'SMRG04'
lkid = 'SMRG05'
infl = 'SMRG     LOGPRINT'
CALL srchlog  */
 
/* VPL */
lkid = 'VPLRECV'
infl = 'VPLRECV  LOG'
CALL srchrtn2 'AuthCode', 'AUTHCODE', evpl
infl = 'PVP      LOGPRINT'
CALL srchlog
lkid = 'VPLDUMP'
infl = 'PVP      LOGPRINT'
CALL srchlog
infl = 'VPLSYNC  SYNC_LOG'
CALL srchlog
lkid = 'VPLTRK'
infl = 'VPL      LOGPRINT'
CALL srchlog
lkid = 'VPLLOG'
infl = 'VPLLOG   LOG'
CALL srchlog
lkid = 'VPLWRK01'
infl = 'PVP      LOGPRINT'
CALL srchrtn3 'CP Link Failed', 'A tape request is being initiated',
              'Tape volume not found', evpl
lkid = 'VPLWRK02'
CALL srchrtn3 'CP Link Failed', 'A tape request is being initiated',
              'Tape volume not found', evpl
lkid = 'VPLWRK03'
CALL srchlog
lkid = 'VPLWRK04'
CALL srchlog
lkid = 'VPLWRK05'
CALL srchlog
lkid = 'VPLWRK06'
CALL srchlog
lkid = 'VPLWRK07'
CALL srchrtn1 'Sql Error rc:8 sqlcode:-712', evpl
lkid = 'VPLWRK08'
CALL srchlog
lkid = 'VPLWRK09'
CALL srchlog
lkid = 'VPLWRK10'
CALL srchlog
lkid = 'VPLWRK11'
CALL srchlog
lkid = 'VPLWRK12'
CALL srchlog
lkid = 'VPLWRK13'
CALL srchlog
lkid = 'VPLWRK14'
CALL srchlog
lkid = 'VPLWRK15'
CALL srchlog
lkid = 'VPLWRK16'
CALL srchlog
 
/* check rdr files & logids */
CALL ckrdrmvs
CALL ckrdrvpl
CALL ckidvpl
 
/* check disk usage by appl group */
Call ckgdu mvs   90 emvs
Call ckgdu vm    90 evm
Call ckgdu vse   90 evse
Call ckgdu ses   90 eses
Call ckgdu smrg  90 esmr
Call ckgdu aix   90 eaix
Call ckgdu cati  90 ecat
Call ckgdu as40  90 eas4
Call ckgdu work  90 ewrk
Call ckgdu vpl   90 evpl
 
DO_SENDF:
Parse Var emon usid '@' domn
'SENDFILE' ofle 'TO' usid 'AT' domn,
  "(SMTP ASCII SUBJ '"rttl rtme"'"
 
'SET MSG OFF';'SET IMSG ON';'SET EMSG ON';'SET WNG ON';'SET SM ON'
 
Exit
 
prntrep:
 verd = GET_VER()
 hdr0 = 'Report Name: MONLOG' verd
 hdr1 = 'Run Title  :' rttl rtme
 hdr2 = 'Search Date:' sdte
 'PIPE Literal ',
    '| Literal' hdr2,
    '| Literal' hdr1,
    '| Literal' hdr0,
    '| >' ofle,
    '| Console'
Return
 
linkacc:
 'PIPE CP LINK' lkid lkdv vdev 'RR'
 If RC=0 Then
   'PIPE CMS ACCESS' vdev mode '(MODE0'
 If RC\=0 Then Do
    ermsg='Unable to Link/Access:' lkid lkdv
   'PIPE CP DETACH' vdev
   'PIPE Literal ',
   '| Literal' ermsg,
   '| >>' ofle
 End
Return
 
prnthdr:
 'PIPE literal INFILE=' || infl,
   '| literal LINKID=' || LEFT(lkid,8) lkdv dates TIME('N'),
   '| > ' oflh,
   '| >>' ofle
Return
 
srchlog:
 CALL linkacc
 CALL prnthdr
 'PIPE',
   '<' infl mode,
   '| Locate (W1) (' || sdte || '(',
   '| Locate (W5) /E:/',
   '| Strip Trailing',
   '| >>' ofle
 'RELEASE' vdev '(DETACH'
Return
 
srchlog1:
 Parse Arg srch1
 'PIPE',
  '<' infl mode,
  '| Locate (W1) (' || sdte || '(',
  '| Locate (W5) /E:/',
  '| Strip Trailing',
  '| >>' ofle,
  '| Locate /' || srch1 || '/',
  '| >>' oflw
Return
 
srchlog2:
 Parse Arg srch1, srch2
 'PIPE (endchar ?)',
  '<' infl mode,
  '| Locate (W1) (' || sdte || '(',
  '| Locate (W5) /E:/',
  '| Strip Trailing',
  '| >>' ofle,
  '| a: Locate /' || srch1 || '/',
  '| f: faninany',
  '| >>' oflw,
  '?',
  'a:',
  '| Locate /' || srch2 || '/',
  '| f:'
Return
 
srchlog3:
 Parse Arg srch1, srch2, srch3
 'PIPE (endchar ?)',
  '<' infl mode,
  '| Locate (W1) (' || sdte || '(',
  '| Locate (W5) /E:/',
  '| Strip Trailing',
  '| >>' ofle,
  '| a: Locate /' || srch1 || '/',
  '| f: faninany',
  '| >>' oflw,
  '?',
  'a:',
  '| b: Locate /' || srch2 || '/',
  '| f:',
  '?',
  'b:',
  '| Locate /' || srch3 || '/',
  '| f:'
Return
 
srchrtn1:
 Parse Arg srch1, eml
 CALL linkacc
 CALL prnthdr
 'ERASE' oflw
 CALL srchlog1 srch1
 'RELEASE' vdev '(DETACH'
 'STATE' oflw
 If RC = 0 Then Do
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml eml, oflm, lkid lkdv
   End
Return
 
srchrtn2:
 Parse Arg srch1, srch2, eml
 CALL linkacc
 CALL prnthdr
 'ERASE' oflw
 CALL srchlog2 srch1, srch2
 'RELEASE' vdev '(DETACH'
 'STATE' oflw
 If RC = 0 Then Do
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml eml, oflm, lkid lkdv
   End
Return
 
srchrtn3:
 Parse Arg srch1, srch2, srch3, eml
 CALL linkacc
 CALL prnthdr
 'ERASE' oflw
 CALL srchlog3 srch1, srch2, srch3
 'RELEASE' vdev '(DETACH'
 'STATE' oflw
 If RC = 0 Then Do
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml eml, oflm, lkid lkdv
   End
Return
 
mvsrtn:
 lkid = 'MVSERR'
 CALL linkacc
 err = 0
 infl = 'PDO* weekno'
 CALL prnthdr
 'ERASE' oflw
 Call CALGWKNO udte 'U'
 yywk = Result
 If DATATYPE(yywk,'W') \= 1 Then Do
   emes  = yywk
   ermsg = '*** ERROR:' emes
   CALL prnterr
   err = err + 1
   End
 Else Do
   infl = 'PDOCNTX ' yywk
   CALL chkfile infl mode
   infl = 'PDO'||yywk 'LISTING'
   CALL chkfile infl mode
   End
 If err > 0 Then Do
   Parse Value Time() With hh':'mm':'ss  /*@1.3d*/
   If hh<7 | (hh=7 & mm<15) Then Return  /*@1.3d*/
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml emvs, oflm, lkid lkdv
   End
 'RELEASE' vdev '(DETACH'
Return
 
vmrtn:
 CALL linkacc
 'PIPE (endchar ?)',
   '<' infl mode,
   '| Locate (w1) (' || sdte || '(',
   '| a: Locate /VMSERVIC .. I: Sleeping till next rundate/',
   '| f: faninany',
   '| VAR xbeg',
   '?',
   'a:',
   '| Locate /VMSERVIC .. I: Processing started for Rundate/',
   '| f:'
 If xbeg = 'XBEG' Then Do
   'ERASE' oflw
   emes  = 'VMSERVIC not running. Restart the process immediately.'
   ermsg = '*** ERROR:' emes
   CALL prnterr
   CALL sndeml evm, oflw, '--' emes
   End
 'RELEASE' vdev '(DETACH'
Return
 
vsertn:
 srch1 = 'not found in VSE.MFMASTER'
 srch2 = 'class (OCO or UNC) not identified'
 eml   = evse
 lkid = 'VSESTAT'
 infl = 'VSE      LOGPRINT'
 CALL linkacc
 CALL prnthdr
 'ERASE' oflw
 CALL srchlog2 srch1, srch2
 'STATE' oflw
 If RC = 0 Then Do
   CALL vsertn2
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml eml, oflm, lkid lkdv
   End
 'RELEASE' vdev '(DETACH'
Return
 
vsertn2:
 infl = 'SERVICE  RECEIVE'
 'PIPE literal INFILE=' || infl,
   '| >>' oflw
 'PIPE',
  '<' infl mode,
  '| Locate (W6) (' || udte || '(',
  '| >>' oflw
Return
 
wrkstrtn:
 lkdv = '191'
 lkid = 'WORKSTCP'
 infl = 'WORKSTCP LOGPRINT'
 CALL srchlog
 infl = 'TCPDISK  LOG'
 CALL srchlog
 
 lkdv = '591'
 CALL linkacc
 pwfl = 'PASSWORD FILE'
 infl = pwfl; /* excf='FTP_AL EXEC'          @1.02c */
 CALL prnthdr
 'ERASE' oflw
 err = 0
 CALL chkfile pwfl mode
/* CALL chkfile excf mode                    @1.02c */
 CALL chkpswd 'ftp_al'
 CALL chkpswd 'ftp_acq'
 CALL chkpswd 'ftp_ptfc'
 If err > 0 Then Do
   CALL mrgfiles oflh, oflw, oflm
   CALL sndeml ewrk, oflm, lkid lkdv
   End
 'RELEASE' vdev '(DETACH'
Return
 
smrgrtn:
 lkid = 'SMRG01'
 lkdv = '191'
 infl = 'SMASAP   LOG'
 wlvl = 99
 wdif = 10
 CALL linkacc
 CALL prnthdr
 'PIPE CMS Listfile' infl mode '(NOH FUL | Spec W8 1 | Var fdte'
 If RC=0 Then Do
    If sdte=Right(fdte,10,0) & srcherwa()>0 Then Do
       'ERASE' oflt
       Do i=1 To rec.0
          Parse Var rec.i . '(' usg . max ')' .
          pct = Format(usg/max*100,3,0)
          dif = max - usg
          If pct >= wlvl & dif <= wdif Then
             'PIPE Literal' Left(rec.i,65) pct'%' dif'rem | >>' oflt
       End
       'STATE' oflt
       If RC = 0 Then Do
          'ERASE' oflw
          emes  = 'Overflow subset/s greater/equal' wlvl'% full.'
          ermsg = '*** WARNING:' emes
          CALL prnterr
          'PIPE Literal  | >>' oflw
          CALL mrgfiles oflw, oflt, oflm
          CALL sndeml esmr, oflm, '--' emes
       End
    End
 End
 'RELEASE' vdev '(DETACH'
Return
 
srcherwa:
 rec.0=0
 'PIPE (endchar ?)',
  '<' infl mode,
  '| a: Casei Find error',
  '| Strip Trailing',
  '| >>' ofle,
  '| f: faninany',
  '?',
  'a:',
  '| Casei Find warning',
  '| Casei Zone 9-* Loc /is nearly full/',
  '| Stem rec.',
  '| f:'
Return rec.0
 
chkfile:
 Parse Arg ckfl
 'STATE' ckfl
 If RC <> 0 then Do
   emes  = 'RC='RC 'Unable to access required file -' ckfl
   ermsg = '*** ERROR:' emes
   CALL prnterr
   err = err + 1
   End
Return
 
chkpswd:
 Parse Arg ident .
 locstr = '%"admin/' || ident || '%'
 'PIPE',
   '<' pwfl mode,
   '| Locate (W1)' locstr,
   '| SPECs Words 2 1',
   '| STRIP BOTH /"/',
   '| VAR pswd'
 Parse Upper Var pswd pwdu
 If pwdu <> pswd then Do
   emes  = 'String required to be in uppercase -' ident pswd
   ermsg = '*** ERROR:' emes
   CALL prnterr
   err = err + 1
   End
Return
 
prnterr:
 'PIPE Literal '      '| >>' ofle
 'PIPE Literal' ermsg '| >>' ofle '| >>' oflw
 'PIPE Literal '      '| >>' ofle
Return
 
mrgfiles: PROCEDURE                 /* Merge files */
 Parse Arg inf1, inf2, omf
 'PIPE <' inf1,
  '| APPEND <' inf2,
  '| >' omf
Return
 
sndeml: PROCEDURE Expose rttl
 Parse Arg usid '@' domn, ofls, ssub
 'SENDFILE' ofls 'TO' usid 'AT' domn,
   "(SMTP ASCII SUBJ '"rttl ssub"'"
Return
 
ckrdrmvs:
 err=0
 'ERASE' oflt
 CALL ckrdrfl 'MMVSCLOS', oflt
 CALL ckrdrfl 'S76MBLD ', oflt
 CALL ckrdrfl 'PTFRECVR', oflt
 CALL ckrdrfl 'PTFRECV ', oflt
 CALL ckrdrfl 'MVSAUTH ', oflt
 CALL ckrdrfl 'MVSERR  ', oflt
 If err > 0 Then Do
    'ERASE' oflw
    emes  = 'MVS RDR files found waiting to be processed.'
    ermsg = '*** WARNING:' emes
    CALL prnterr
    CALL mrgfiles oflw, oflt, oflm
    CALL sndeml emvs, oflm, '--' emes
 End
Return
 
ckrdrvpl:
 err=0
 'ERASE' oflt
 CALL ckrdrfl 'PH24PTFS', oflt    /* VM          */
 CALL ckrdrfl 'PTFNET  ', oflt    /* VSE         */
 CALL ckrdrfl 'AIXRCV  ', oflt    /* AIX         */
 CALL ckrdrfl 'XPFRCV  ', oflt    /* AS/400      */
 CALL ckrdrfl 'PTFRCV  ', oflt    /* CATIA       */
 CALL ckrdrfl 'WORKSRCV', oflt    /* WORKSTATION */
/*CALL ckrdrfl 'MMVSCLOS', oflt*/  /* MVS         */
 CALL ckrdrfl 'vplrecv ', oflt    /* VPL         */
 CALL ckrdrfl 'vpltrk  ', oflt    /* VPL         */
 CALL ckrdrfl 'ptfstat ', oflt    /* VSE Deltas  */
 CALL ckrdrfl 'vmtrack ', oflt    /* VM  Deltas  */
 CALL ckrdrfl 'ptfstat1', oflt    /* VPL Excess  */
 CALL ckrdrfl 'vplio   ', oflt    /* VPL Books   */
 If err > 0 Then Do
    'ERASE' oflw
    emes  = 'RDR files found waiting to be processed.'
    ermsg = '*** WARNING:' emes
    CALL prnterr
    CALL mrgfiles oflw, oflt, oflm
    CALL sndeml evpl, oflm, '--' emes
    End
Return
 
ckrdrfl: PROCEDURE Expose err
 Arg usid, ofls
 'PIPE CP QUERY FILES' usid,
  '| var vqf',
  '| SPEC w2 1',
  '| var vrf'
 If DATATYPE(vrf,'W') & vrf > 9 Then Do
    err = err + 1
   'PIPE CP QUERY' usid,
    '| var vqu',
    '| Literal' vqf,
    '| Literal ',
    '| >>' ofls
    If Pos('not logged on',vqu) = 0 Then
      'PIPE',
       '| Literal Process may have stopped and may need recycling.',
       '| >>' ofls
 End
Return
 
ckidvpl:
 Call CALQVPL oflx
 'ERASE' oflt
 'PIPE <' oflx,
  '| Loc /not logged on/',
  '| Nlocate /VPLADMIN/',
  '| Nlocate /VPLIO/',
  '| >>' oflt
 'STATE' oflt
 If RC = 0 Then Do
    'ERASE' oflw
    emes  = 'VPL process/machine found not logged on.'
    ermsg = '*** ERROR:' emes
    CALL prnterr
    'PIPE Literal  | >>' oflw
    CALL mrgfiles oflw, oflt, oflm
    CALL sndeml evpl, oflm, '--' emes
    End
Return
 
ckgdu: /* check disk usage: report usage >= warning level */
 Parse Arg grp wlvl eml .
 Parse Var oflx vfn vft vfm
 outf = vfn'.'vft'.'vfm
 'EXEC CALGDU' . . 'group='grp 'ofile='outf 'nbrowse'
 'PIPE <' oflx '| Stem gdu.'
 'PIPE Literal' gdu.1 '| >' oflt
 err=0
 Do i=2 to gdu.0
    Parse Var gdu.i . . . usge .
    If usge >= wlvl Then Do
       'PIPE Literal' gdu.i '| >>' oflt
       err = err + 1
    End
 End
 If err > 0 Then Do
    'ERASE' oflw
    Upper grp
    s=''; If err>1 Then s='s'
    emes  = grp 'disk's 'found greater/equal' wlvl'% full.'
    ermsg = '*** WARNING:' emes
    CALL prnterr
    'PIPE Literal  | >>' oflw
    CALL mrgfiles oflw, oflt, oflm
    CALL sndeml eml, oflm, '--' emes
    End
Return
 
SHOW_DOC:
 odoc = sysn 'DOC A'
 'PIPE',
   '<' sysn syst sysm,
   '| TOLABel  * End-doc',
   '| >' odoc
 'PIPE CMS BROWSE' odoc
 Exit
 
SHOW_VER:
 verd = GET_VER()
 Say sysn verd
 Exit
 
GET_VER:
 'PIPE',
   '<' sysn syst sysm,
   '| FRLABel  * ver.rel',
   '| TOLABel  * End-doc',
   '| SPECs 4;21 1',
   '| STRIP',
   '| Locate',
   '| Stem verdte.'
 i = verdte.0
 Return verdte.i
