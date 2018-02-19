      program puder
c
c --- Still another cell parameter refinement program.
c     started development back in the old days, 1985 or even 
c     earlier in the very first versions.
c
      include 'puder.cmn'

      character command*80, cpar*80
c
c --- logical unit numbers.
c
      lukeys = 5
      ludisp = 6
      lubat  = 7
      luinp  = 8
      luout  = 9
      lulog  = 10
      lubat2 = 11
c
c --- some other default values.
c
      iloop = 1
      ilogfl = 0
      logfile_open = .false.
c
c --- default values for some other parameters.
c
      call resall
c
c --- write welcome text and start looping...
c
      write (ludisp,'(a)') 
     >' Welcome to PUDER, version: 2017-11-29'
      write (ludisp,'(a)') ' '

    1 continue
      write (ludisp,'(a,$)') ' Puder>'
      if (ilogfl .eq. 1) then
         write(lulog,'(a)') ' '
      endif

      read  (lukeys,'(a)') cpar
      if (cpar .eq. ' ') then
         goto 1
      endif

      call strip(cpar)
      call del_comment (cpar)
      if (cpar .eq. ' ') then
         goto 1
      endif
      call move_first_element (cpar,command)
      call upline(command)

      if (command .eq. 'FILE' .or.
     >    command .eq. 'FIL' .or.
     >    command .eq. 'FI' ) then
c
c ------ process commands from a file.
c
         call cmd_from_file (cpar)

      else
c
c ------ process commands from the keyboard.
c
         call interp (command,cpar,iloop)

      endif
      if (iloop .eq. 1) then
         goto 1
      endif

      end

c-----------------------------------------------------------------------

      subroutine cmd_from_file (cmdpar)

      include 'puder.cmn'

      character cmdpar*80,cmd*80,fname*80
      integer   traced_lines

      if (trace_flag) then
         traced_lines = 0
      endif

      if (cmdpar .eq. ' ') then
         write (ludisp,'(a,$)') ' Which file ?  '
         read  (lukeys,'(a)') fname
      else
         fname = cmdpar
      endif

      if (fname .ne. ' ') then
         n = lastch(fname)
         open (lubat,file=fname(1:n),status='old',
     >         iostat=istat)
         if (istat .eq. 0) then
c
c --------- this is the actual read loop for processing commands
c           from a file.
c
            iloop = 1
 1          continue
            read (lubat,'(a)',end=2) cmdpar
            if (trace_flag) then
               traced_lines = traced_lines+1
            endif
c
c --------- process non-blank command line.
c
            if (cmdpar .ne. ' ') then
               write(ludisp,'(a,a)') ' ',cmdpar(1:lastch(cmdpar))
               call strip (cmdpar)
               call del_comment (cmdpar)
               if (cmdpar .ne. ' ') then
                  call move_first_element (cmdpar,cmd)
                  if (cmdpar .ne. ' ') then
                     call strip (cmdpar)
                  endif
                  call upline (cmd)
                  if (cmd .eq. 'FILE' .or.
     >                cmd .eq. 'FIL' .or.
     >                cmd .eq. 'FI' ) then
c
c ------------------- process commands from a file.
c
                      call cmd_2_from_file (cmdpar)

                  else
c
c ------------------- process commands from the keyboard.
c
                      call interp (cmd,cmdpar,iloop)

                  endif
                  if (iloop .eq. 1) then
                     goto 1
                  endif
                  if (trace_flag .and. 
     >                mod(traced_lines,20) .eq. 0) then
                     call more_routine(ludisp,traced_lines)
                  endif
               endif
            else
               write(ludisp,'(a)') ' '
            endif
            if (iloop .eq. 1) then
               goto 1
            endif
            close (lubat)
            write(ludisp,'(a)') ' End of input from instruction file.'
            return
         else 
            write(ludisp,'(a)') ' Error opening this filee.'
         endif

    2    continue
         close (lubat)
         write(ludisp,'(a)') 
     >   ' Unexpected end of instruction file...'
         
      else
         write(ludisp,'(a)')
     >   'Blank filenames are not allowed...'

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine cmd_2_from_file (cmdpar)

      include 'puder.cmn'

      character cmdpar*80,cmd*80,fname*80
      integer   traced_lines

      if (trace_flag) then
         traced_lines = 0
      endif

      if (cmdpar .eq. ' ') then
         write (ludisp,'(a,$)') ' Which file ?  '
         read  (lukeys,'(a)') fname
      else
         fname = cmdpar
      endif

      if (fname .ne. ' ') then
         n = lastch(fname)
         open (lubat2,file=fname(1:n),status='old',
     >         iostat=istat)
         if (istat .eq. 0) then
c
c --------- this is the actual read loop for processing commands
c           from a file.
c
            imore = 1
 1          continue
            read (lubat2,'(a)',end=2) cmdpar
            if (trace_flag) then
               traced_lines = traced_lines+1
            endif
c
c --------- process non-blank command line.
c
            if (cmdpar .ne. ' ') then
               write(ludisp,'(a,a)') ' ',cmdpar(1:lastch(cmdpar))
               call strip (cmdpar)
               call del_comment (cmdpar)
               if (cmdpar .ne. ' ') then
                  call move_first_element (cmdpar,cmd)
                  if (cmdpar .ne. ' ') then
                     call strip (cmdpar)
                  endif
                  call upline (cmd)
                  call interp (cmd,cmdpar,imore)
                  if (trace_flag .and. 
     >                mod(traced_lines,20) .eq. 0) then
                     call more_routine(ludisp,traced_lines)
                  endif
               endif
            else
               write(ludisp,'(a)') ' '
            endif
            if (imore .eq. 1) then
               goto 1
            endif
            close (lubat2)
            write (ludisp,'(a)') ' End of input from instruction file.'
            return
         else 
            write (ludisp,'(a)') ' Error opening this file...'
         endif

    2    continue
         close (lubat2)
         write (ludisp,'(a)') ' Unexpected end of instruction file...'
         
      else
         write (ludisp,'(a)') ' Blank filenames are not allowed...'

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine getchar(ch)
      character ch*1
      read(*,'(a)') ch
      return
      end

c-----------------------------------------------------------------------

      subroutine more_routine(ludisp,lines)
      character ch*1
      write(ludisp,'(a,$)') ' -------------------- More ?'
    1 continue
      call getchar(ch)
      if (ichar(ch) .eq. 13) then
         write(ludisp,'(a,a,$)') ' ',char(13)
         lines = lines-1
      elseif (ichar(ch) .eq. 32) then
         write(ludisp,'(a,a,$)') ' ',char(13)
         lines = 0
      else
         goto 1
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine write_error_string(idev,str)
      character str*80
      write(idev,'(a)') str(1:lastch(str))
      return
      end

c-----------------------------------------------------------------------

      subroutine interp (command,cpar,iflag)
c
c --- this routine interprets a command and possibly also some optional
c     parameters to that very command.
c
      include 'puder.cmn'

      character command*80, cpar*80, ch*1

      if (command .eq. '2THETA' .or.
     >    command .eq. '2THET' .or.
     >    command .eq. '2THE' .or.
     >    command .eq. '2TH' .or.
     >    command .eq. '2T' .or.
     >    command .eq. 'TWOTHETA' .or.
     >    command .eq. 'TWOTHET' .or.
     >    command .eq. 'TWOTHE' .or.
     >    command .eq. 'TWOTH' .or.
     >    command .eq. 'TWOT' .or.
     >    command .eq. 'TWO' .or.
     >    command .eq. 'TW' ) then
         ispctyp = 4

      elseif (command .eq. 'ADJUST' .or.
     >        command .eq. 'ADJUS' .or.
     >        command .eq. 'ADJU' .or.
     >        command .eq. 'ADJ' .or.
     >        command .eq. 'AD' ) then
         call adjust_lines (cpar)

      elseif (command .eq. 'CALC' .or.
     >        command .eq. 'CAL' .or.
     >        command .eq. 'CA' ) then
         call calcul        

      elseif (command .eq. 'CELL' .or.
     >        command .eq. 'CEL' .or.
     >        command .eq. 'CE' ) then
         call rdcell (cpar)

      elseif (command .eq. 'CLOSE' .or.
     >        command .eq. 'CLOS' .or.
     >        command .eq. 'CLO' .or.
     >        command .eq. 'CL' ) then
         close (lulog)
         logfile_open = .false.
         if (ilogfl .eq. 1) then
            ilogfl = 0
            write(ludisp,'(a)')
     >      ' Logging disabled, when logfile is closed.'
         endif

      elseif (command .eq. 'COMMENT' .or.
     >        command .eq. 'COMMEN' .or. 
     >        command .eq. 'COMME' .or. 
     >        command .eq. 'COMM' .or. 
     >        command .eq. 'COM' ) then
         call process_comments (ludisp,lulog,ilogfl,cpar)

      elseif (command .eq. 'CONDITION' .or.
     >        command .eq. 'CONDITIO' .or.
     >        command .eq. 'CONDITI' .or.
     >        command .eq. 'CONDIT' .or.
     >        command .eq. 'CONDI' .or.
     >        command .eq. 'COND' .or.
     >        command .eq. 'CON' ) then
         call exttop (lukeys,ludisp,numext,ixs,ilatt,cpar)

      elseif (command .eq. 'CORRELATION' .or.
     >        command .eq. 'CORRELATIO' .or.
     >        command .eq. 'CORRELATI' .or.
     >        command .eq. 'CORRELAT' .or.
     >        command .eq. 'CORRELA' .or.
     >        command .eq. 'CORREL' .or.
     >        command .eq. 'CORRE' .or.
     >        command .eq. 'CORR' .or.
     >        command .eq. 'COR' ) then
         call write_correlation_matrix

      elseif (command .eq. 'CREATE' .or.
     >        command .eq. 'CREAT' .or.
     >        command .eq. 'CREA' .or.
     >        command .eq. 'CRE' .or.
     >        command .eq. 'CR' ) then
         call create_new_file (cpar)

      elseif (command .eq. 'CYCLE' .or.
     >        command .eq. 'CYCL' .or.
     >        command .eq. 'CYC' .or.
     >        command .eq. 'CY' ) then
         call cycref (cpar)

      elseif (command .eq. 'DATA' .or.
     >        command .eq. 'DAT' .or.
     >        command .eq. 'DA' ) then
         iweg = 0
         call get_data (cpar,iweg)

      elseif (command .eq. 'DELTA' .or.
     >        command .eq. 'DELT' .or.
     >        command .eq. 'DEL' .or.
     >        command .eq. 'DE' ) then
         call chadelta(lukeys,ludisp,delta_twotheta,cpar)

      elseif (command .eq. 'ZERO' .or.
     >        command .eq. 'ZER' .or.
     >        command .eq. 'ZE' ) then
         call chazero(lukeys,ludisp,cpar,qobs,nobs,wave)

      elseif (command .eq. 'DVALUES' .or.
     >        command .eq. 'DVALUE' .or.
     >        command .eq. 'DVALU' .or.
     >        command .eq. 'DVAL' .or.
     >        command .eq. 'DVA' .or.
     >        command .eq. 'DV' ) then
         ispctyp = 5

      elseif (command .eq. 'DOS' .or.
     >        command .eq. 'DO' .or.
     >        command .eq. 'D' .or.
     >        command .eq. 'UNIX' .or.
     >        command .eq. '%' .or.
     >        command .eq. 'CMD' ) then
      call exec_cmd_command(lukeys,ludisp,'DOS  ',cpar)

      elseif (command .eq. 'LS' ) then
         call exec_cmd_command(lukeys,ludisp,'LS   ',cpar) 

      elseif (command .eq. 'LL' ) then
         call exec_cmd_command(lukeys,ludisp,'LL   ',cpar) 

      elseif (command .eq. 'CLS' ) then
         call exec_cmd_command(lukeys,ludisp,'CLS  ',cpar) 

      elseif (command .eq. 'MKDIR' ) then
         call exec_cmd_command(lukeys,ludisp,'MKDIR',cpar) 

      elseif (command .eq. 'RMDIR' ) then
         call exec_cmd_command(lukeys,ludisp,'RMDIR',cpar) 

      elseif (command .eq. 'DEL' .or.
     >        command .eq. 'RM' ) then
         call exec_cmd_command(lukeys,ludisp,'DEL  ',cpar) 

      elseif (command .eq. 'MOVE' .or.
     >        command .eq. 'MV' ) then
         call exec_cmd_command(lukeys,ludisp,'MOVE ',cpar) 

      elseif (command .eq. 'COPY' .or.
     >        command .eq. 'CP' ) then
         call exec_cmd_command(lukeys,ludisp,'COPY ',cpar) 

      elseif (command .eq. 'PUSH' ) then
c         call exec_cmd_command(lukeys,ludisp,'PUSH ',cpar) 

      elseif (command .eq. 'MORE' ) then
         call exec_cmd_command(lukeys,ludisp,'MORE ',cpar) 

      elseif (command .eq. 'ESD' .or.
     >        command .eq. 'ES' ) then
         call decide_wheter_to_use_esdobs(cpar)

      elseif (command .eq. 'EXIT' .or.
     >        command .eq. 'EXI' .or.
     >        command .eq. 'EX' .or.
     >        command .eq. 'END' .or.
     >        command .eq. 'EN' ) then
         if (ilogfl .eq. 1) then
            close(lulog)
         endif
         iflag = 0

      elseif (command .eq. 'GRAF' ) then
        call do_graphical_investigation

      elseif (command .eq. 'GIJ') then
         call write_gij_parameters

      elseif (command .eq. 'GROUP' .or.
     >        command .eq. 'GROU' .or.
     >        command .eq. 'GRO' ) then
         call get_space_group_extictions (cpar)


      elseif (command .eq. 'EXPORT' .or.
     >        command .eq. 'EXPOR' .or.
     >        command .eq. 'EXPO' .or.
     >        command .eq. 'EXP' ) then
         call export_data(cpar)

      elseif (command .eq. 'IMPORT' .or.
     >        command .eq. 'IMPOR' .or.
     >        command .eq. 'IMPO' .or.
     >        command .eq. 'IMP' .or.
     >        command .eq. 'IM' ) then
         call import_data(cpar)

      elseif (command .eq. 'INDEX' .or.
     >        command .eq. 'INDE' .or.
     >        command .eq. 'IND' ) then
         call indlin (cpar)

      elseif (command .eq. 'INTERACTIVE' .or.
     >        command .eq. 'INTERACTIV' .or.
     >        command .eq. 'INTERACTI' .or.
     >        command .eq. 'INTERACT' .or.
     >        command .eq. 'INTERAC' .or.
     >        command .eq. 'INTERA' .or.
     >        command .eq. 'INTER' .or.
     >        command .eq. 'INTE' .or.
     >        command .eq. 'INT' ) then
         call index_very_interactively

      elseif (command .eq. 'INVERSE' .or.
     >        command .eq. 'INVERS' .or.
     >        command .eq. 'INVER' .or.
     >        command .eq. 'INVE' .or.
     >        command .eq. 'INV' ) then
         call write_inverse_matrix

      elseif (command .eq. 'HKLDATA' .or.
     >        command .eq. 'HKLDAT' .or.
     >        command .eq. 'HKLDA' .or.
     >        command .eq. 'HKLD' ) then
         iweg = 0
         call get_hkls_data (cpar,iweg)

      elseif (command .eq. 'LATTICE' .or.
     >        command .eq. 'LATTIC' .or.
     >        command .eq. 'LATTI' .or.
     >        command .eq. 'LATT' .or.
     >        command .eq. 'LAT' .or.
     >        command .eq. 'LA' ) then
         call setlat (lukeys,ludisp,ilatt,cpar)

      elseif (command .eq. 'LOG') then
         call toglog(ilogfl,lukeys,ludisp,lulog,logfile_open,cpar)

      elseif (command .eq. 'LOCK' .or.
     >        command .eq. 'LOC' ) then
         call lockli(cpar)

      elseif (command .eq. 'LSQSUM' ) then
         call write_lsqsum

      elseif (command .eq. 'MERIT' .or.
     >        command .eq. 'MERI' .or.
     >        command .eq. 'MER' .or.
     >        command .eq. 'ME' ) then
         call merits (cpar)

      elseif (command .eq. 'NOTRACE' .or.
     >        command .eq. 'NOTRAC' .or.
     >        command .eq. 'NOTRA' .or.
     >        command .eq. 'NOTR' .or.
     >        command .eq. 'NOT' .or.
     >        command .eq. 'NO' ) then
         trace_flag = .false.

      elseif (command .eq. 'OPEN' .or.
     >        command .eq. 'OPE' .or.
     >        command .eq. 'OP' ) then
         call open_log_file
     >        (lukeys,ludisp,lulog,logfile_open,cpar,istat)
         write(ludisp,'(a)')
     >   ' Use LOG to enable logging to this log-file.'

      elseif (command .eq. 'PCELL' .or.
     >        command .eq. 'PCEL' .or.
     >        command .eq. 'PCE' .or.
     >        command .eq. 'PC' ) then
         call prtsome(1)

      elseif (command .eq. 'PCELL*' .or.
     >        command .eq. 'PCEL*' .or.
     >        command .eq. 'PCE*' .or.
     >        command .eq. 'PC*' ) then
         call prtsome(2)

      elseif (command .eq. 'PRINT' .or.
     >        command .eq. 'PRIN' .or.
     >        command .eq. 'PRI' .or.
     >        command .eq. 'PR' ) then
         call prtsome(0)

      elseif (command .eq. 'QVALUES' .or.
     >        command .eq. 'QVALUE' .or.
     >        command .eq. 'QVALU' .or.
     >        command .eq. 'QVAL' .or.
     >        command .eq. 'QVA' .or.
     >        command .eq. 'QV' ) then
         ispctyp = 2

      elseif (command .eq. 'REDUCE' .or.
     >        command .eq. 'REDUC' .or.
     >        command .eq. 'REDU' .or.
     >        command .eq. 'RED' ) then
         call reduce
         call prtsome(1)

      elseif (command .eq. 'REFINE' .or.
     >        command .eq. 'REFIN' .or.
     >        command .eq. 'REFI' .or.
     >        command .eq. 'REF' ) then
         call refine
         call prtsome(1)

      elseif (command .eq. 'RESET' .or. 
     >        command .eq. 'RESE' .or. 
     >        command .eq. 'RES' ) then
         call resall

      elseif (command .eq. 'RPN' .or.
     >        command .eq. 'RP' ) then
         call rpn (lukeys,ludisp)

      elseif (command .eq. 'SET' ) then
         call set_parameter (cpar)

      elseif (command .eq. 'SHOW' ) then
         call show_parameter (cpar)

      elseif (command .eq. 'SETHKL' .or.
     >        command .eq. 'SETHK' .or.
     >        command .eq. 'SETH' ) then 
         call sethkl

      elseif (command .eq. '2DZONE' .or.
     >        command .eq. '2DZON' .or.
     >        command .eq. '2DZO' .or.
     >        command .eq. '2DZ' ) then 
         call search_2d_zone (cpar)

      elseif (command .eq. 'SSQVALUES' .or.
     >        command .eq. 'SSQVALUE' .or.
     >        command .eq. 'SSQVALU' .or.
     >        command .eq. 'SSQVAL' .or.
     >        command .eq. 'SSQVA' .or.
     >        command .eq. 'SSQV' .or.
     >        command .eq. 'SSQ' .or.
     >        command .eq. 'SSQTHETA' .or.
     >        command .eq. 'SSQTHET' .or.
     >        command .eq. 'SSQTHE' .or.
     >        command .eq. 'SSQTH' .or.
     >        command .eq. 'SSQT' ) then
         ispctyp = 1

      elseif (command .eq. 'STATUS' .or.
     >        command .eq. 'STATU' .or.
     >        command .eq. 'STAT' .or.
     >        command .eq. 'STA' .or.
     >        command .eq. 'ST' ) then
         call shstat(ludisp)
         if (ilogfl .eq. 1) then
            call shstat(lulog)
         endif

      elseif (command .eq. 'SYSTEM' .or.
     >        command .eq. 'SYSTE' .or.
     >        command .eq. 'SYST' .or.
     >        command .eq. 'SYS' .or.
     >        command .eq. 'SY' ) then
         call setsys (lukeys,ludisp,isyst,cpar)

      elseif (command .eq. 'THETA' .or.
     >        command .eq. 'THET' .or.
     >        command .eq. 'THE' .or.
     >        command .eq. 'TH' ) then
         ispctyp = 3

      elseif (command .eq. 'TRACE' .or.
     >        command .eq. 'TRAC' ) then
         trace_flag = .true.

      elseif (command .eq. 'TREOR' .or.
     >        command .eq. 'TREO' .or.
     >        command .eq. 'TRE' ) then
         call ab_initio_treor (cpar)

      elseif (command .eq. 'TRANSFORM' .or.
     >        command .eq. 'TRANSFOR' .or.
     >        command .eq. 'TRANSFO' .or.
     >        command .eq. 'TRANSF' .or.
     >        command .eq. 'TRANS' .or.
     >        command .eq. 'TRAN' ) then
         call celtra

      elseif (command .eq. 'UNLOCK' .or.
     >        command .eq. 'UNLOC' .or.
     >        command .eq. 'UNLO' .or.
     >        command .eq. 'UNL' .or.
     >        command .eq. 'UN'  ) then
         call unlock(cpar)

      elseif (command .eq. 'USE' .or.
     >        command .eq. 'US' ) then
         call setuse (lukeys,ludisp,linuse,cpar)

      elseif (command .eq. 'WDATA' .or.
     >        command .eq. 'WDAT' .or.
     >        command .eq. 'WDA' .or.
     >        command .eq. 'WD' ) then
         iweg = 1
         call get_data (cpar,iweg)

      elseif (command .eq. 'WHKLDATA' .or.
     >        command .eq. 'WHKLDAT' .or.
     >        command .eq. 'WHKLDA' .or.
     >        command .eq. 'WHKLD' ) then
         iweg = 1
         call get_hkls_data (cpar,iweg)

      elseif (command .eq. 'VERBOSE' .or.
     >        command .eq. 'VERBOS' .or.
     >        command .eq. 'VERBO' .or.
     >        command .eq. 'VERB' .or.
     >        command .eq. 'VER' .or.
     >        command .eq. 'VE' ) then
         if (iverb .eq. 0) then
            iverb = 1
            write(ludisp,'(a)')
     >      ' Verbose printing in the indexing phase.'
         else
            iverb = 0
            write(ludisp,'(a)')
     >      ' No verbose printing in the indexing phase.'
         endif

      elseif (command .eq. 'WAVE' .or.
     >        command .eq. 'WAV' .or.
     >        command .eq. 'WA' ) then
         call setwave (wave,lukeys,ludisp,cpar)

      elseif (command .eq. 'WEIGHT' .or.
     >        command .eq. 'WEIGH' .or.
     >        command .eq. 'WEIG' .or.
     >        command .eq. 'WEI' .or.
     >        command .eq. 'WE' ) then
         call setweg (cpar)

      elseif (command .eq. 'WRDEF' .or.
     >        command .eq. 'WRDE' .or.
     >        command .eq. 'WRD' ) then
         if (cpar .eq. ' ') then
            wr_default = 
     >      'n h k l qo qc qd 2tho 2thc 2thd nokay'
         elseif (cpar .eq. '?') then
            call wrdef_elements (ludisp)
         else
            wr_default = cpar
         endif

      elseif (command .eq. 'WRITE' .or.
     >        command .eq. 'WRIT' .or.
     >        command .eq. 'WRI' ) then
         if (cpar .eq. ' ') then
            call sqwrit (wr_default)
         else
            call sqwrit (cpar)
         endif

      elseif (command .eq. 'HELP' .or.
     >        command .eq. 'HEL' .or.
     >        command .eq. 'HE' .or.
     >        command .eq. 'H' ) then
         call fnhelp(ludisp)

      elseif (command .eq. 'H00' ) then
         ihset = 0
         ikset = 1
         ilset = 1
         ikmin = 0
         ikmax = 0
         ilmin = 0
         ilmax = 0
         write(ludisp,'(a)') ' Only H00 zone will be indexed.'         

      elseif (command .eq. '0K0' ) then
         ihset = 1
         ikset = 0
         ilset = 1
         ihmin = 0
         ihmax = 0
         ilmin = 0
         ilmax = 0
         write(ludisp,'(a)') ' Only 0K0 zone will be indexed.'         

      elseif (command .eq. '00L' ) then
         ihset = 1
         ikset = 1
         ilset = 0
         ihmin = 0
         ihmax = 0
         ikmin = 0
         ikmax = 0
         write(ludisp,'(a)') ' Only 00L zone will be indexed.'         

      elseif (command .eq. 'HK0' ) then
         ihset = 0
         ikset = 0
         ilset = 1
         ilmin = 0
         ilmax = 0
         write(ludisp,'(a)') ' Only HK0 zone will be indexed.'         

      elseif (command .eq. '0KL' ) then
         ihset = 1
         ikset = 0
         ilset = 0
         ihmin = 0
         ihmax = 0
         write(ludisp,'(a)') ' Only 0KL zone will be indexed.'         

      elseif (command .eq. 'H0L' ) then
         ihset = 0
         ikset = 1
         ilset = 0
         ikmin = 0
         ikmax = 0
         write(ludisp,'(a)') ' Only H0L zone will be indexed.'         

      elseif (command .eq. 'HKL' ) then
         ihset = 0
         ikset = 0
         ilset = 0
         write(ludisp,'(a)') ' All of HKL-space will be indexed.'         

      else
         write(ludisp,'(a)') 
     >   ' Unknown command.  Press RETURN to continue...'
         call getchar(ch)

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine exec_cmd_command (lukeys,ludisp,str1,str2)
c
c --- GNU fortran model of this routine.
c
      integer   status
      integer   lukeys,ludisp
      character str1*5, str2*80, str*90

      if (str1 .eq. 'DOS' .or.
     >    str1 .eq. 'CMD' .or.
     >    str1 .eq. '%' .or.
     >    str1 .eq. 'UNIX' ) then
         if (str2 .eq. ' ') then
            write (ludisp,'(a,$)') ' CMD: '
            read (lukeys,'(a)') str2
         endif
         if (str2 .ne. ' ') then
            status = system (str2(1:lastc(str2)))
            write(ludisp,'(a,i4)') 
     >      ' Status returned from CMD is:',status
         endif

      elseif (str1 .eq. 'LS' ) then
         if (str2 .eq. ' ') then
            str = 'ls'
         else
            str = 'ls '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'LL' ) then
         if (str2 .eq. ' ') then
            str = 'ls -l'
         else
            str = 'ls -l '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'MKDIR' ) then
         if (str2 .eq. ' ') then
            write(ludisp,'(a)') ' You must supply a directory name.'
         else
            str = 'mkdir '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'RMDIR' ) then
         if (str2 .eq. ' ') then
            write(ludisp,'(a)') ' You must supply a directory name.'
         else
            str = 'rmdir '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'DEL' .or.
     >        str1 .eq. 'RM' ) then
         if (str2 .eq. ' ') then
            write(ludisp,'(a)') ' You must supply a file name.'
         else
            str = 'rm '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'MOVE' .or.
     >        str1 .eq. 'MV' ) then
         if (str2 .eq. ' ') then
            write(ludisp,'(a)') ' You must supply two file names.'
         else
            str = 'mv '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'COPY' .or.
     >        str1 .eq. 'CP' ) then
         if (str2 .eq. ' ') then
            write(ludisp,'(a)') ' You must supply two file names.'
         else
            str = 'cp '//str2(1:lastc(str2))
         endif
         status = system (str)
         write(ludisp,'(a,i4)') ' Status returned is:',status

      elseif (str1 .eq. 'MORE') then
         status = system ('more '//str2(1:lastc(str2)))
         write(ludisp,'(a,i4)') ' Status returned is:',status

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine fnhelp(id)

      character carr(100)*15

      data carr(1)  /'2THETA         '/
      data carr(2)  /'TWOTHETA       '/
      data carr(3)  /'ADJUST         '/
      data carr(4)  /'CALC           '/
      data carr(5)  /'CELL           '/
      data carr(6)  /'CLOSE          '/
      data carr(7)  /'COMMENT        '/
      data carr(8)  /'CONDITION      '/
      data carr(9)  /'CORRELATION    '/
      data carr(10) /'CREATE         '/
      data carr(11) /'CYCLE          '/
      data carr(12) /'DATA           '/
      data carr(13) /'DELTA          '/
      data carr(14) /'DVALUES        '/
      data carr(15) /'ESD            '/
      data carr(16) /'EXIT           '/
      data carr(17) /'END            '/
      data carr(18) /'GROUP          '/
      data carr(19) /'EXPORT         '/
      data carr(20) /'IMPORT         '/
      data carr(21) /'INDEX          '/
      data carr(22) /'INTERACTIVE    '/
      data carr(23) /'INVERSE        '/
      data carr(24) /'HKLDATA        '/
      data carr(25) /'LATTICE        '/
      data carr(26) /'LOG            '/
      data carr(27) /'LOCK           '/
      data carr(28) /'LSQSUM         '/
      data carr(29) /'MERIT          '/
      data carr(30) /'NOTRACE        '/
      data carr(31) /'OPEN           '/
      data carr(32) /'PCELL          '/
      data carr(33) /'PCELL*         '/
      data carr(34) /'PRINT          '/
      data carr(35) /'QVALUES        '/
      data carr(36) /'REFINE         '/
      data carr(37) /'RESET          '/
      data carr(38) /'RPN            '/
      data carr(39) /'SET            '/
      data carr(40) /'SHOW           '/
      data carr(41) /'SETHKL         '/
      data carr(42) /'SSQVALUES      '/
      data carr(43) /'SSQTHETA       '/
      data carr(44) /'STATUS         '/
      data carr(45) /'SYSTEM         '/
      data carr(46) /'THETA          '/
      data carr(47) /'TRACE          '/
      data carr(48) /'TRANSFORM      '/
      data carr(49) /'UNLOCK         '/
      data carr(50) /'USE            '/
      data carr(51) /'WDATA          '/
      data carr(52) /'WHKLDATA       '/
      data carr(53) /'VERBOSE        '/
      data carr(54) /'WAVE           '/
      data carr(55) /'WEIGHT         '/
      data carr(56) /'WRDEF          '/
      data carr(57) /'WRITE          '/
      data carr(58) /'CMD            '/
      data carr(59) /'LS             '/
      data carr(60) /'LL             '/
      data carr(61) /'MKDIR          '/
      data carr(62) /'RMDIR          '/
      data carr(63) /'DEL            '/
      data carr(64) /'RM             '/
      data carr(65) /'MOVE           '/
      data carr(66) /'MV             '/
      data carr(67) /'COPY           '/
      data carr(68) /'CP             '/
      data carr(69) /'PUSH           '/
      data carr(70) /'HELP           '/
      data carr(71) /'H00            '/
      data carr(72) /'0K0            '/
      data carr(73) /'00L            '/
      data carr(74) /'HK0            '/
      data carr(75) /'0KL            '/
      data carr(76) /'H0L            '/
      data carr(77) /'HKL            '/
      data carr(78) /'2DZONE         '/
      data carr(79) /'GRAF           '/

      write(id,'(a)') ' '
      write(id,'(a)') 
     >' Available commands to PUDER, see manual for description.'
      write(id,'(a)') ' '
      write(id,'(1x,5a)') (carr(i),i=1,79)
      write(id,'(a)') ' '

      return
      end

c-----------------------------------------------------------------------

      subroutine set_parameter (cmdpar)

      include 'puder.cmn'

      character parname*80, parvalue*80, cmdpar*80
c
c --- Get parameter-name and -value from command parameter.
c
      if (cmdpar .eq. ' ') then
         write(ludisp,'(a)') 
     >   ' You must enter a parameter name and a corresponding value.'
         write(ludisp,'(a)') 
     >   ' Use the command SHOW ALL to get a list of available names.'
         return
      endif
c
c --- check for possible question mark as command parameter.
c
      call move_first_element (cmdpar,parname)
      if (parname .eq. '?') then
         parname = 'ALL'
         call show_parameter(parname)
         return
      endif

      if (cmdpar .eq. ' ') then
         write(ludisp,'(a)') 
     >   ' You must enter a value to the parameter also, try again!'
         return
      else
c
c ------ set the value of the actual parameter.
c        begin with a copying of the first iten in the paraneter string.
c
         call move_first_element (cmdpar,parvalue)
c
c ------ decode a possible numerical value given as parameter.
c
         call get_real(parvalue,temp_value,istat)
         if (istat .ne. 0) then
            write(ludisp,'(a,i5)')
     >      ' Error during read of parameter value, IOSTAT=',istat
            return
         endif
      endif
c
c --- check the parameter name.
c
      call upline (parname)

      if (parname .eq. 'DELTA') then
         if (temp_value .lt. 0.0) then
            write(ludisp,'(a)') ' Too small delta value'
            return
         endif
         delta_twotheta = temp_value

      elseif (parname .eq. 'HKL' ) then
         call sethkl

      elseif (parname .eq. 'WAVE') then
         wave = temp_value
         cl2q = wave*wave*0.25

      else
         write (ludisp,'(a,a,a)')
     >   ' Unknown parameter identifier /',
     >   parname(1:lastc(parname)),'/'

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine show_parameter (cmdpar)

      include 'puder.cmn'

      character parname*80, cmdpar*80
c
c --- Get parameter-name from command parameter.
c
      do while (cmdpar .ne. ' ')
         call move_first_element (cmdpar,parname)
c
c ------ show the value of the actual parameter.
c
         call upline (parname)

         if (parname .eq. 'DELTA') then
            write(ludisp,'(a,f10.4)') ' DELTA ........: ',delta_twotheta

         elseif (parname .eq. 'WAVE') then
            write(ludisp,'(a,f10.4)') ' WAVE .........: ',wave

         elseif (parname .eq. 'ALL') then
            write(ludisp,'(a,f13.7)') ' DELTA ........: ',delta_twotheta
            write(ludisp,'(a,f13.7)') ' WAVE .........: ',wave

         else
            write (ludisp,'(a,a,a)') 
     >      ' Unknown parameter identifier /',
     >      parname(1:lastc(parname)),'/...'
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine decide_wheter_to_use_esdobs(cmdpar)

      include 'puder.cmn'

      character cmdpar*80

      if (cmdpar .ne. ' ') then
         call strip(cmdpar)
         call upline(cmdpar)
      endif
      if (cmdpar .eq. 'YES' .or.
     >    cmdpar .eq. 'YE' .or.
     >    cmdpar .eq. 'Y' .or.
     >    cmdpar .eq. 'ON' ) then
         use_esdobs_in_weights = .true.

      elseif (cmdpar .eq. 'NO' .or.
     >        cmdpar .eq. 'N' .or.
     >        cmdpar .eq. 'OFF' .or.
     >        cmdpar .eq. 'OF' ) then
         use_esdobs_in_weights = .false.

      else
         if (cmdpar .ne. ' ') then
            write(ludisp,'(a,a,a)') 
     >      ' Unknown parameter given /',cmdpar(1:lastch(cmdpar)),'/'
         endif
      endif

      if (use_esdobs_in_weights) then
         write(ludisp,'(a)') 
     >   ' ESD of observed spacing used when calculated weights.'
         if (ilogfl .eq. 1) then
            write(lulog,'(a)') 
     >      ' ESD of observed spacing used when calculated weights.'
         endif
      else
         write(ludisp,'(a,a)') 
     >   ' Only ordinary weights used in refinements,',
     >   ' no ESD of observed spacing used.'
         if (ilogfl .eq. 1) then
            write(ludisp,'(a,a)') 
     >      ' Only ordinary weights used in refinements,',
     >      ' no ESD of observed spacing used.'
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine adjust_lines (str)

      include 'puder.cmn'

      real      one_ninth
      real      kvot, low_kvot, high_kvot, qerr(20)
      character answer*1, str*80
      logical   flag, adjust_all_lines

      one_ninth = 1.0/9.0

      if (nobs .eq. 0) then
         write (ludisp,'(a)') ' No spacing data stored...'
         return
      endif
      if (str .ne. ' ') then
         call strip(str)
         call upline(str)
         if (str .eq. 'ALL' .or. str .eq. 'AL' .or. str .eq. 'A') then
            adjust_all_lines = .true.
         else
            write(ludisp,'(a,a,a)')
     >      ' Unknown argument /',str(1:lastch(str)),'/'
            adjust_all_lines = .false.
         endif
      else
         adjust_all_lines = .false.
      endif     
      flag = .false.
      answer = ' '

      n = min(nobs,20)
      do i = 1,n

         if (qobs(i) .lt. 0.05) then
            qerr(i) = 0.0002
         else
            qerr(i) = 0.0004
         endif
      enddo
      do i1  = 1,n-1
         do i2 = i1+1,n

            kvot = qobs(i2)/qobs(i1)
            low_kvot = (qobs(i2)-qerr(i2))/(qobs(i1)+qerr(i1))
            high_kvot = (qobs(i2)+qerr(i2))/(qobs(i1)-qerr(i1))

            if (high_kvot .ge. 4.0 .and. low_kvot .le. 4.0) then
               flag = .true.
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,i2,a,f6.4,a,i2,a)')
     >         ' QOBS:',i2,' = ',kvot,' * QOBS:',i1,
     >         ' which is nearly a factor of 4.'

               temp_old_twotheta = q_2_twoth(qobs(i1),wave)
               temp_new_twotheta = q_2_twoth(0.25*qobs(i2),wave)

               write(ludisp,'(a,i2,a,f8.4,a,f8.4,a,f8.4)')
     >         ' 2theta:',i1,' = ',temp_old_twotheta,
     >         ' before change and ',
     >         temp_new_twotheta,' after change. Adj.=',
     >         temp_new_twotheta-temp_old_twotheta

               temp_old_dvalue = q_2_d(qobs(i1))
               temp_new_dvalue = q_2_d(0.25*qobs(i2))

               write(ludisp,'(a,i2,a,f8.4,a,f8.4,a,f8.4)')
     >         ' Dvalue:',i1,' = ',temp_old_dvalue,
     >         ' before change and ',
     >         temp_new_dvalue,' after change.Adj.=',
     >         temp_new_dvalue-temp_old_dvalue

               if (.not. adjust_all_lines) then
                  write(ludisp,'(a,$)') 
     >            ' Correct the first line ? <Y/N[Def.] or Q(uit)> '
                  read(lukeys,'(a)') answer
               endif
               if (answer .eq. 'y' .or. answer .eq. 'Y' .or.
     >             adjust_all_lines) then
                  qnew = qobs(i2)*0.25
                  write(ludisp,'(a,i2,a,f8.6,a,f8.6)')
     >            ' QOBS:',i1,' = ',qobs(i1),' ==> ',qnew
                  qobs(i1) = qnew
               elseif (answer .eq. 'q' .or. answer .eq. 'Q') then
                  write(ludisp,'(a)') ' '
                  return
               endif

            elseif (high_kvot .ge. 9.0 .and. low_kvot .le. 9.0) then
               flag = .true.
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,i2,a,f6.4,a,i2,a)')
     >         ' QOBS:',i2,' = ',kvot,' * QOBS:',i1,
     >         ' which is nearly a factor of 9.'

               temp_old_twotheta = q_2_twoth(qobs(i1),wave)
               temp_new_twotheta = q_2_twoth(one_ninth*qobs(i2),wave)

               write(ludisp,'(a,i2,a,f8.4,a,f8.4,a,f8.4)')
     >         ' 2theta:',i1,' = ',temp_old_twotheta,
     >         ' before change and ',
     >         temp_new_twotheta,' after change. Adj.=',
     >         temp_new_twotheta-temp_old_twotheta

               temp_old_dvalue = q_2_d(qobs(i1))
               temp_new_dvalue = q_2_d(one_ninth*qobs(i2))

               write(ludisp,'(a,i2,a,f8.4,a,f8.4,a,f8.4)')
     >         ' Dvalue:',i1,' = ',temp_old_dvalue,
     >         ' before change and ',
     >         temp_new_dvalue,' after change.Adj.=',
     >         temp_new_dvalue-temp_old_dvalue

               if (.not. adjust_all_lines) then
                  write(ludisp,'(a,$)') 
     >            ' Correct the first line ? <Y/N[Def.] or Q(uit)> '
                  read(lukeys,'(a)') answer
               endif
               if (answer .eq. 'y' .or. answer .eq. 'Y' .or.
     >             adjust_all_lines) then
                  qnew = qobs(i2)*one_ninth
                  write(ludisp,'(a,i2,a,f8.6,a,f8.6)')
     >            ' QOBS:',i1,' = ',qobs(i1),' ==> ',qnew
                  qobs(i1) = qnew
               elseif (answer .eq. 'q' .or. answer .eq. 'Q') then
                  write(ludisp,'(a)') ' '
                  return
               endif

            endif
         enddo
      enddo

      if (flag) then
         write(ludisp,'(a)') ' '
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine process_comments (ludisp,lulog,ilogfl,str)

      character str*80

      if (str .eq. ' ') then
         write(ludisp,'(a)') ' '
         if (ilogfl .eq. 1) then
            write(lulog,'(a)') ' '
         endif
      else
         call strip(str)
         write(ludisp,'(a,a)') ' ',str(1:lastch(str)) 
         if (ilogfl .eq. 1) then
            write(lulog,'(a,a)') ' ',str(1:lastch(str))
         endif
      endif

      return
      end

c-----------------------------------------------------------------------
      subroutine chazero(lukeys,ludisp,str,qobs,nobs,wave)

      character str*80,answer*1

      if (str .eq. ' ') then
         write (ludisp,'(a,$)') 
     >   ' Give new ZERO-point in 2theta to add to observed lines :'
         read (lukeys,'(a)') str
         if (str.eq.' ') then
            write(ludisp,'(a)') ' No change done ...'
            return
         endif
      endif

      call strip(str)

      call get_real (str,temp,istat)
      if (istat .ne. 0) then
         write(ludisp,'(a)')
     >   ' Error during numeric read, no change done ...'
      else
         write(ludisp,'(a,f10.4,a)') 
     >   ' New ZERO-point error = ',temp,' degrees in 2theta'
         zeropoint = temp
         endif
         write(ludisp,'(a)') ' '  
         write(ludisp,'(a)') 
     >   ' Are you sure you want to apply this zero point? <Y/[N]> :'
         read(lukeys,'(a)') answer
         if (answer .eq. 'y' .or. answer .eq. 'Y') then
            do i = 1,nobs
               tobs = q2t (qobs(i),wave)
               qnew = t2q (tobs+0.5*zeropint,wave)
               qobs(i) = qnew
            enddo
         else
            write(ludisp,'(a)') ' OK, no change done...'
            return
         endif
      endif

      return
      end

c-----------------------------------------------------------------------
      function q2t (q,wave)
      q2t = (todeg(asin(0.5*wave*sqrt(qobs(i))))
      return
      end
c-----------------------------------------------------------------------
      function t2q (t,wave)
      t2q = (to(asin(0.5*wave*sqrt(qobs(i))))
      return
      end
c-----------------------------------------------------------------------
      subroutine chadelta(lukeys,ludisp,delta,str)

      character str*80

      if (str .eq. ' ') then
         write (ludisp,'(a,f8.4,a,$)') 
     >   ' Give Delta-twotheta [',delta,'] :'
         read (lukeys,'(a)') str
         if (str.eq.' ') then
            write(ludisp,'(a)') ' No change done ...'
            return
         endif
      endif

      call strip(str)

      call get_real (str,temp,istat)
      if (istat .ne. 0) then
         write(ludisp,'(a)')
     >   ' Error during numeric read, no change done ...'
      else
         if (temp .gt. 0.0 .and. temp .le. 180.0) then
            delta = temp
         else
            write(ludisp,'(a)') 
     >      ' Error, allowed range is: 0 <= delta-twotheta <= 180 deg.'
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_lsqsum

      include 'puder.cmn'

      sum_abs_del_q   = 0.0
      sum_sqr_del_q   = 0.0
      sum_abs_del_2th = 0.0
      sum_sqr_del_2th = 0.0
      sum_abs_del_dv  = 0.0
      sum_sqr_del_dv  = 0.0
      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            del_q = qobs(i)-qcalc(i)
            del_2th = 2.0*(asin(sqrt(cl2q*qobs(i)))-
     >                     asin(sqrt(cl2q*qcalc(i))))
            del_dv  = (1.0/sqrt(qobs(i)) - 1.0/sqrt(qcalc(i)))
            sum_abs_del_q   = sum_abs_del_q + abs(del_q)
            sum_sqr_del_q   = sum_sqr_del_q + sqr(del_q)
            sum_abs_del_2th = sum_abs_del_2th + abs(del_2th)
            sum_sqr_del_2th = sum_sqr_del_2th + sqr(del_2th)
            sum_abs_del_dv  = sum_abs_del_dv  + abs(del_dv)
            sum_sqr_del_dv  = sum_sqr_del_dv  + sqr(del_dv)
         endif
      enddo
      write(ludisp,1) ' Sum(abs(delta_Q))   = ',sum_abs_del_q
      write(ludisp,3) ' Sum(sqr(delta_Q))   = ',sum_sqr_del_q
      write(ludisp,2) ' Sum(abs(delta_2th)) = ',todeg(sum_abs_del_2th)
      write(ludisp,3) ' Sum(sqr(delta_2th)) = ',todeg(sum_sqr_del_2th)
      write(ludisp,1) ' Sum(abs(delta_dv))  = ',sum_abs_del_dv
      write(ludisp,3) ' Sum(sqr(delta_dv))  = ',sum_sqr_del_dv
    1 format(a,f12.7)
    2 format(a,f12.5)
    3 format(a,g12.5)
      if (ilogfl .eq. 1) then
         write(lulog,1) ' Sum(abs(delta_Q))   = ',sum_abs_del_q
         write(lulog,3) ' Sum(sqr(delta_Q))   = ',sum_sqr_del_q
         write(lulog,2) ' Sum(abs(delta_2th)) = ',todeg(sum_abs_del_2th)
         write(lulog,3) ' Sum(sqr(delta_2th)) = ',todeg(sum_sqr_del_2th)
         write(lulog,1) ' Sum(abs(delta_dv))  = ',sum_abs_del_dv
         write(lulog,3) ' Sum(sqr(delta_dv))  = ',sum_sqr_del_dv
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_correlation_matrix

      include 'puder.cmn'

      if (n_corr_mat .lt. 1 .or. n_corr_mat .gt. 6) then
         write(ludisp,'(a)') 
     >   ' Correlation matrix not defined, use REFINE first...'
      else
         write(ludisp,'(a)') ' '
         write(ludisp,'(a)') ' Correlation matrix:'
         write(ludisp,'(a)') ' '
         do irow = 1,n_corr_mat
            write(ludisp,'(6f12.6)') 
     >      (corr_mat(irow,icol),icol=1,n_corr_mat)
         enddo
         write(ludisp,'(a)') ' '

         if (ilogfl .eq. 1) then
            write(lulog,'(a)') ' '
            write(lulog,'(a)') ' Correlation matrix:'
            write(lulog,'(a)') ' '
            do irow = 1,n_corr_mat
               write(lulog,'(6f12.6)') 
     >         (corr_mat(irow,icol),icol=1,n_corr_mat)
            enddo
            write(lulog,'(a)') ' '
         endif

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_inverse_matrix

      include 'puder.cmn'

      if (n_corr_mat .lt. 1 .or. n_corr_mat .gt. 6) then
         write(ludisp,'(a)') 
     >   ' Inverse Hess-matrix not defined, use REFINE first...'
      else
         write(ludisp,'(a)') ' '
         write(ludisp,'(a)') ' Inverse Hess-matrix:'
         write(ludisp,'(a)') ' '
         do irow = 1,n_corr_mat
            write(ludisp,'(6g12.5)') 
     >      (amat(irow,icol),icol=1,n_corr_mat)
         enddo
         write(ludisp,'(a)') ' '

         if (ilogfl .eq. 1) then
            write(lulog,'(a)') ' '
            write(lulog,'(a)') ' Inverse Hess-matrix:'
            write(lulog,'(a)') ' '
            do irow = 1,n_corr_mat
               write(lulog,'(6g12.5)') 
     >         (amat(irow,icol),icol=1,n_corr_mat)
            enddo
            write(lulog,'(a)') ' '
         endif

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine get_space_group_extictions (str)

      include 'puder.cmn'

      character str*80, part1*80, part2*80, part3*80, part4*80

      if (str .eq. ' ') then
         write(ludisp,'(a,$)') 
     >   ' Enter full space-group symbol: (L p q r)'
         read (lukeys,'(a)') str
      endif
      if (str .eq. ' ') then
         return
      else
         space_group_symbol = str(1:lastch(str))
         write(ludisp,'(3a)') 
     >   ' Space group symbol = /',
     >   space_group_symbol(1:lastch(space_group_symbol)),'/'
      endif
      call move_first_element (space_group_symbol, part1)
      ipart1 = 0
      ipart2 = 1
      ipart3 = 1
      ipart4 = 1
      if (str .ne. ' ') then
         call move_first_element (space_group_symbol, part2)
         ipart2 = 0
      endif
      if (str .ne. ' ') then
         call move_first_element (space_group_symbol, part3)
         ipart3 = 0
      endif
      if (str .ne. ' ') then
         call move_first_element (space_group_symbol, part4)
         ipart4 = 0
      endif
      if (ipart1+ipart2+ipart3+ipart4 .eq. 0 .and.
     >    space_group_symbol .eq. ' ') then
         write(ludisp,'(a)') ' Decoding space group symbol'
         write(ludisp,'(a)') ' '
         write(ludisp,'(2a)') ' Part 1 = ',part1(1:lastch(part1))
         call setlat (lukeys,ludisp,ilatt,part1)
         write(ludisp,'(2a)') ' Part 2 = ',part2(1:lastch(part2))
c         call exttop (lukeys,ludisp,numext,ixs,ilatt,cpar)

         write(ludisp,'(2a)') ' Part 3 = ',part3(1:lastch(part3))
         write(ludisp,'(2a)') ' Part 4 = ',part4(1:lastch(part4))
      else
         write(ludisp,'(a)') ' Error decoding this space group symbol'
      endif

      return
      end
      
c-----------------------------------------------------------------------

      subroutine import_data(str)

      include 'puder.cmn'

      character str*80, typstr*80, fname*80, title*80
      logical   flag

      if (str .eq. ' ') then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' Enter type and filename!'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' Known types are: Q, SSQ, D and 2TH'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' try again...'
         write (ludisp,'(a)') ' '
         return
      endif
c
c --- type of file to import.
c
      call move_first_element (str, typstr)
      call strip(typstr)
      call upline(typstr)

      if (typstr .eq. 'Q') then
         ityp = 1
      elseif (typstr .eq. 'SSQ') then
         ityp = 2
      elseif (typstr .eq. 'D') then
         ityp = 3
      elseif (typstr .eq. '2TH') then
         ityp = 4
      elseif (typstr .eq. '?') then
         write(ludisp,'(a)') 'Known file types: Q, SSQ, D and 2TH.'
         return
      else
         write(ludisp,'(a,a,a)') 
     >   ' Unknown file type /',typstr(1:lastc(typstr)),'/'
         return
      endif
c
c --- name of file
c
      if (str .eq. ' ') then
         write(ludisp,'(a)') ' '
         write(ludisp,'(a)') ' Blank filenames are not allowed...'
         write(ludisp,'(a)') ' '
         return
      endif
      call move_first_element (str, fname)
      call strip(fname)

      open (luinp,file=fname,status='unknown',iostat=istat)
      if (istat .ne. 0) then
         write(ludisp,'(a)') ' '
         write (ludisp,'(a)') 
     >   ' Unable to open this file /',fname(1:lastc(fname)),'/'
         write(ludisp,'(a)') ' '
         return
      endif

c      write(ludisp,'(a,i2)') ' Ityp = ',ityp
c      write(ludisp,'(3a)')   ' Ityp = /',fname(1:lastc(fname)),'/'
c
c --- read the content of the file.
c     first the title line...
c
      read(luinp,'(a)') title
      if (title .ne. ' ') then
         write(ludisp,'(a,a)') ' Title:',title(1:lastc(title))
      else
         write(ludisp,'(a)') 
     >   ' No title given on first line of input file'
      endif
c
c --- and then the rest of the data
c
      flag = .true.
      do while (flag)
         read(luinp,*,iostat=istat) temp
         if (istat .eq. 0) then
            nobs=nobs+1
            if (ityp .eq. 1) then
               qobs(nobs) = temp
            elseif (ityp .eq. 2) then
               qobs(nobs) = ssq_2_q(temp,wave)
            elseif (ityp .eq. 3) then
               qobs(nobs) = d_2_q(temp)
            elseif (ityp .eq. 4) then
               qobs(nobs) = twoth_2_q(temp,wave)
            endif
         else
            flag = .false.
         endif
      enddo

      close(luinp)

      return
      end

c-----------------------------------------------------------------------

      subroutine export_data(str)

      include 'puder.cmn'

      character str*80, typstr*80, fname*80

      if (str .eq. ' ') then
         write (ludisp,'(a)')   ' '
         write (ludisp,'(a)')   ' SYNTAX: Export type filename'
         write (ludisp,'(a)')   ' '
         write (ludisp,'(a)')   ' Known file types are:'
         write (ludisp,'(a)')   ' '
         write (ludisp,'(a)')   ' Q ....: Q-values (Q = 1/d^2).'
         write (ludisp,'(a)')   ' SSQ ..: Sine SQuare theta.'
         write (ludisp,'(a)')   ' TRE ..: TREOR input file.'
         write (ludisp,'(a)')   ' ITO ..: ITO input file.'
         write (ludisp,'(a)')   ' LOU ..: DICVOL06 input file.'
         write (ludisp,'(a)')   ' M ....: MATLAB / Octave file.'
         write (ludisp,'(a)')   ' '
         write (ludisp,'(a)')   
     >  ' All file types will contain a title string as the first line.'
         write (ludisp,'(a)')   ' '
         return
      endif
c
c --- type of file to export.
c
      call move_first_element (str, typstr)
      call strip(typstr)
      call upline(typstr)

      if     (typstr .eq. 'SSQ') then
         ityp = 1
      elseif (typstr .eq. '2TH') then
         ityp = 2
      elseif (typstr .eq. 'TRE') then
         ityp = 3
      elseif (typstr .eq. 'LOU') then
         ityp = 4
      elseif (typstr .eq. 'Q') then
         ityp = 5
      elseif (typstr .eq. 'M') then
         ityp = 6
      elseif (typstr .eq. 'ITO') then
         ityp = 7
      elseif (typstr .eq. '?') then
         write(ludisp,'(a)') 
     >  'Known file types: Q, SSQ, 2TH, TRE, ITO, LOU and M'
         return
      else
         write(ludisp,'(a,a,a)') 
     >   ' Unknown file type /',typstr(1:lastc(typstr)),'/'
         return
      endif
c
c --- name of file
c
      if (str .eq. ' ') then
         write(ludisp,'(a)') ' '
         write(ludisp,'(a)') ' Blank filenames are not allowed...'
         write(ludisp,'(a)') ' '
         return
      endif
      call move_first_element (str, fname)
      call strip(fname)
      open (luout,file=fname,status='unknown',iostat=istat)
      if (istat .ne. 0) then
         write(ludisp,'(a)') ' '
         write (ludisp,'(a)') 
     >   ' Unable to open this file /',fname(1:lastc(fname)),'/'
         write(ludisp,'(a)') ' '
         return
      endif
c
c --- write the content depending on the file.
c
      if (ityp .eq. 1) then
         write(luout,'(a)') ' SSQ-file exported from PUDER'
         do i = 1,nobs
            write(luout,'(f16.8,f10.4)') 
     >      q_2_ssq(qobs(i),wave),freevar(i)
         enddo
         
      elseif (ityp .eq. 2) then
         write(luout,'(a)') ' 2TH-file exported from PUDER'
         do i = 1,nobs
            write(luout,'(f10.4)') q_2_twoth(qobs(i),wave)
         enddo

      elseif (ityp .eq. 3) then
         write(luout,'(a)') ' TREOR-file exported from PUDER'
         do i = 1,nobs
            write(luout,'(f10.4)') q_2_twoth(qobs(i),wave)
         enddo
         write(luout,'(a)') ' '
         write(luout,'(a,f7.5,a)') 'WAVE=',wave,','
         write(luout,'(a)') 'CHOICE=3, VOL=-2000, END* '

      elseif (ityp .eq. 4) then
         write(luout,'(a)') ' DICVOL-06 file exported from PUDER'
         write(luout,'(8i8)') nobs,2,1,1,1,1,0,0
         write(luout,'(7f8.1)') 25.0,25.0,25.0,10.0,2500.0,90.0,125.0
         write(luout,'(f8.5,3f8.4)') wave,0.0,0.0,0.0
         write(luout,'(2f8.2,3i8)') 0.03,10.0,-1,1,1
         do i = 1,nobs
            write(luout,'(f10.4)') q_2_twoth(qobs(i),wave)
         enddo

      elseif (ityp .eq. 5) then
         write(luout,'(a)') ' Q-file (Q=1/d2) exported from PUDER'
         do i = 1,nobs
            write(luout,'(f16.8,f10.4)') qobs(i),freevar(i)
         enddo

      elseif (ityp .eq. 6) then
         write(luout,'(a)') '% M-file exported from PUDER.'
         write(luout,'(a)') 
     >   '% 2theta values saved to the array xobs and xcalc. '
         write(luout,'(a)') ' '
         write(luout,'(a)') 'xobs = [ '
         do i = 1,nobs
            write(luout,'(f10.4)') 
     >      q_2_twoth(qobs(i),wave)
         enddo
         write(luout,'(a)') ' ] '

         write(luout,'(a)') ' '
         write(luout,'(a)') 'xcalc = [ '
         do i = 1,nobs
            write(luout,'(f10.4)') 
     >      q_2_twoth(qcalc(i),wave)
         enddo
         write(luout,'(a)') ' ] '

         write(luout,'(a)') ' '
         write(luout,'(a)') '% compute difference '
         write(luout,'(a)') ' '
         write(luout,'(a)') 'xdiff = xobs - xcalc'
         write(luout,'(a)') ' '

         write(ludisp,'(a)') 
     >   ' Script file *.m created...'
         write(ludisp,'(a)') 
     >   ' Execute it with Octave and thereafter use "pudcorr.m"'

      elseif (ityp .eq. 7) then
         write(luout,'(a)') 'ITO file exported from PUDER'
         write(luout,'(a)') ' '
         write(luout,'(a)') ' '
         do i = 1,nobs
            write(luout,'(f10.5)') q_2_twoth(qobs(i),wave)
         enddo
         write(luout,'(a)') ' '
         write(luout,'(a)') 'END '
      endif
      close(luout)

      return
      end

c-----------------------------------------------------------------------

      subroutine create_new_file (str)

      include 'puder.cmn'

      character str*80, fname*80

      if (str .eq. ' ') then
         write (ludisp,'(a,$)') 'Enter filename:'
         read (lukeys,'(a)') str
      endif
      if (str .eq. ' ') then
         write (ludisp,'(a)') 'Ok, no file created...'
         return
      endif
      call move_first_element (str, fname)
      open (luout,file=fname,status='unknown',iostat=istat)
      if (istat .ne. 0) then
         write (ludisp,'(a)') 'Unable to open this file ...'
         return
      endif
c
c --- first some creation information in the file.
c
      write(luout,'(a)')
     > '! This file was made by the CREATE command in PUDER.'
      write(luout,'(a)') ' '
c
c --- Crystal system
c
      if (isyst .eq. 1) then
         write(luout,'(a)') 'System CUBIC'
      elseif (isyst .eq. 2) then
         write(luout,'(a)') 'System TRIGONAL'
      elseif (isyst .eq. 3) then
         write(luout,'(a)') 'System TETRAGONAL'
      elseif (isyst .eq. 4) then
         write(luout,'(a)') 'System HEXAGONAL'
      elseif (isyst .eq. 5) then
         write(luout,'(a)') 'System ORTORHOMBIC'
      elseif (isyst .eq. 6) then
         write(luout,'(a)') 'System MONOCLINIC'
      elseif (isyst .eq. 7) then
         write(luout,'(a)') 'System TRICLINIC'
      endif
c
c --- Cell parameter
c
      write (luout,'(a,3f10.5,3f10.4)') 'Cell ',a,b,c,al,be,ga
c
c --- Lattice centering conditions.
c
      if (ilatt .eq. 1) then
         write (luout,'(a)') 'Lattice P'
      elseif (ilatt .eq. 2) then
         write (luout,'(a)') 'Lattice F'
      elseif (ilatt .eq. 3) then
         write (luout,'(a)') 'Lattice I'
      elseif (ilatt .eq. 4) then
         write (luout,'(a)') 'Lattice A'
      elseif (ilatt .eq. 5) then
         write (luout,'(a)') 'Lattice B'
      elseif (ilatt .eq. 6) then
         write (luout,'(a)') 'Lattice C'
      endif
      write(luout,'(a)') ' '
c
c --- Use 2theta as spacing measure.
c
      write(luout,'(a,f10.6)') 'Wave  ',wave
      write(luout,'(a)') '2theta'
      write(luout,'(a)') ' '

      do i=1,nobs
         temp_2theta = q_2_twoth(qobs(i),wave)
         write(luout,'(a,f10.4,f10.4)') 'Data ',temp_2theta, freevar(i)
      enddo

      if (numext .gt. 0) then
         write(luout,'(a)') ' '
      endif
      call extlis (luout,numext,ixs,1)

      write (luout,'(a)') 'End'

      close (luout)      

      return
      end
      
c-----------------------------------------------------------------------

      subroutine rpn (lukeys,ludisp)

      real      s(100)
      integer   isp,lukeys,ludisp,iloop

      character cmd*80

      iloop  = 1
      isp    = 0

      do while (iloop .eq. 1) 
         write (ludisp,'(a,$)') ' RPN>'
         read  (lukeys,'(a)') cmd

         if (cmd .ne. ' ') then
            call rpninterp(cmd,s,isp,ludisp,iloop)
         endif
      enddo

      end

c-----------------------------------------------------------------------

      subroutine rpninterp (str,s,isp,ludisp,iloop)

      dimension s(1)

      character cmd*80,str*80
      
      do while (str .ne. ' ')
c
c ------ copy first element in string STR to string CMD
c
         call strip(str)
         call upline(str)
         call move_first_element(str,cmd)
c
c ------ interpret CMD
c
         if     (cmd .eq. 'SIN') then
            call rpnsin(s,isp,ludisp)

         elseif (cmd .eq. 'COS') then
            call rpncos(s,isp,ludisp)

         elseif (cmd .eq. 'TAN') then
            call rpntan(s,isp,ludisp)
 
         elseif (cmd .eq. 'ASIN') then
            call rpnasin(s,isp,ludisp)

         elseif (cmd .eq. 'ACOS') then
            call rpnacos(s,isp,ludisp)

         elseif (cmd .eq. 'ATAN') then
            call rpnatan(s,isp,ludisp)

         elseif (cmd .eq. 'SQRT') then
            call rpnsqrt(s,isp,ludisp)

         elseif (cmd .eq. 'SQR') then
            call rpnsq(s,isp,ludisp)

         elseif (cmd .eq. 'LOG' .or. cmd .eq. 'LN') then
            call rpnln(s,isp,ludisp)

         elseif (cmd .eq. 'EXP') then
            call rpnexp(s,isp,ludisp)

         elseif (cmd .eq. 'LOG10' .or. cmd .eq. 'LG') then
            call rpnlg(s,isp,ludisp)

         elseif (cmd .eq. 'TENTO') then
            call rpnten(s,isp,ludisp)

         elseif (cmd .eq. 'Y**X' .or. cmd .eq. 'YTOX') then
            call rpnytox(s,isp,ludisp)

         elseif (cmd.eq.'LIST' .or. cmd.eq.'LIS' .or. cmd.eq.'LI') then
            call stlist(s,isp,ludisp)

         elseif (cmd .eq. '+') then
            call rpnadd(s,isp,ludisp)

         elseif (cmd .eq. '-') then
            call rpnsub(s,isp,ludisp)           

         elseif (cmd .eq. '*') then
            call rpnmul(s,isp,ludisp)

         elseif (cmd .eq. '/') then
            call rpndiv(s,isp,ludisp)

         elseif (cmd .eq. 'TORAD' .or. cmd .eq. 'D-R') then
            call rpntorad(s,isp,ludisp)

         elseif (cmd .eq. 'TODEG' .or. cmd .eq. 'R-D') then
            call rpntodeg(s,isp,ludisp)

         elseif (cmd .eq. 'INV' .or. cmd .eq. '1/X') then
            call rpninv(s,isp,ludisp)

         elseif (cmd.eq.'EXIT' .or. cmd.eq.'EXI' .or. cmd.eq.'EX') then
            iloop = 0

         elseif (cmd.eq.'HELP' .or. cmd.eq.'HEL' .or. cmd.eq.'HE') then
            call rpnhlp(ludisp)

         elseif (cmd .eq. 'CLEAR' .or. cmd .eq. 'CLST') then
            isp = 0

         elseif (cmd .eq. 'ENTER' .or. cmd .eq. 'PUSH') then
            temp = s(isp)
            call rpnstore(temp,s,isp)

         else
            call get_real(cmd,temp,istat)
            if (istat .eq. 0) then
               call rpnstore(temp,s,isp)
            else
               write (ludisp,'(a)') ' ?'
            endif
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine rpnstore(elem,s,isp)
      dimension  s(1)
      isp = isp+1
      s(isp)=elem
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnhlp(ludisp)
      write(ludisp,'(a)')
     >' Available commands:'
      write(ludisp,'(a,/,a,/,a,/,a)')
c     x ------++++++------++++++------++++++------++++++------++++++
     >' SIN   COS   TAN   ASIN  ACOS  ATAN  SQR   SQRT  LIST  EXIT  ',
     >' HELP  LOG   LN    LOG10 LG    EXP   TENTO Y**X  YTOX  ',
     >' +     -     *     /     INV   1/X   TORAD D-R   TODEG D-R   ',
     >' CLEAR CLST  ENTER PUSH'
c     x ------++++++------++++++------++++++------++++++------++++++
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnadd(s,isp,ludisp)
      dimension  s(1)
      if (isp .gt. 1) then
         s(isp-1) = s(isp-1)+s(isp)
         isp = isp-1
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Unary stack...'
         write (ludisp,'(a)') ' Nothing to add...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnsub(s,isp,ludisp)
      dimension  s(1)
      if (isp .gt. 1) then
         s(isp-1) = s(isp-1)-s(isp)
         isp = isp-1
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Unary stack...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnmul(s,isp,ludisp)
      dimension  s(1)
      if (isp .gt. 1) then
         s(isp-1) = s(isp-1)*s(isp)
         isp = isp-1
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Unary stack...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpndiv(s,isp,ludisp)
      dimension  s(1)
      if (isp .gt. 1) then
         if (s(isp) .eq. 0.0) then
            write (ludisp,'(a)') 
     >      ' Division by zero, operation not allowed...'
         else
            s(isp-1) = s(isp-1)/s(isp)
            isp = isp-1
            call rpn_wr(s(isp),ludisp)
         endif
      else
         write (ludisp,'(a)') ' Unary stack...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnsin(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = sin(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpntorad(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = torad(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpntodeg(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = todeg(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpninv(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         if (s(isp) .eq. 0.0) then
            write(ludisp,'(a)')
     >      ' Division by zero, operation not allowed.'
         else
            s(isp) = 1.0/s(isp)
            call rpn_wr(s(isp),ludisp)
         endif
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpncos(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = cos(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpntan(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = tan(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnasin(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = asin(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnacos(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = acos(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnatan(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = atan(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnsq(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = s(isp)*s(isp)
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnsqrt(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = sqrt(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnln(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = log(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnexp(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = exp(s(isp))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnlg(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = log(s(isp))/log(10.0)
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnten(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         s(isp) = exp(s(isp)*log(10.0))
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpnytox(s,isp,ludisp)
      dimension  s(1)
      if (isp .gt. 1) then
         s(isp-1) = exp(s(isp)*log(s(isp-1)))
         isp = isp-1
         call rpn_wr(s(isp),ludisp)
      else
         write (ludisp,'(a)') ' Unary stack...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine stlist(s,isp,ludisp)
      dimension  s(1)
      if (isp .ge. 1) then
         n = isp
         do 1 i = 1,isp
            write (ludisp,'(i4,a,g15.7)') n,':',s(i)
            n = n-1
    1    continue
      else
         write (ludisp,'(a)') ' Stack empty...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine rpn_wr(x,idev)
      write (idev,'(t5,g15.7)') x
      return
      end

c-----------------------------------------------------------------------

      subroutine resall
c
c --- set initial values for some of the parameters.
c
      include 'puder.cmn'

      wave   = 1.5405981
      cl2q   = wave*wave*0.25
      delta_twotheta = 0.03
      isyst  = 1 
      ilatt  = 1
      iverb  = 0
      numext = 0
      linuse = 1
      n_corr_mat = 0
      iwgtyp = 1
      use_esdobs_in_weights = .false.
      smallest_acceptable_esd = 0.0001   ! in deg 2theta.
      ispctyp = 0
      wr_default = 'n h k l qo qc qd 2tho 2thc 2thd nokay'
      n_part_to_read = 1
      i_part_typ(1) = 4
      ihset = 0
      ikset = 0
      ilset = 0
      do i = 1,nobs
         ih(i) = 0
         ik(i) = 0
         il(i) = 0
         qobs(i) = 0.0
         qcalc(i) = 0.0
         nokay(i) = 0
         ilock(i) = 0
      enddo
      nobs   = 0
      a      = 10.0
      b      = 10.0
      c      = 10.0
      al     = 90.0
      be     = 90.0
      ga     = 90.0
      siga   = 0.0
      sigb   = 0.0
      sigc   = 0.0
      sigal  = 0.0
      sigbe  = 0.0
      sigga  = 0.0
      call getrec
      call getgij
      titstr = ' '
      trace_flag = .false.
      space_group_symbol = ' '

      return
      end

c-----------------------------------------------------------------------

      subroutine get_int (str,itemp,istat)
      character str*80,s*80
      n = lastch(str)
      s = ' '
      s(80-n+1:80) = str(1:n)
      read (str,'(i80)',iostat=istat) itemp
      return
      end

c-----------------------------------------------------------------------

      subroutine get_real(str,temp,istat)
      character str*80
      read (str,'(f80.0)',iostat=istat) temp
      return
      end

c-----------------------------------------------------------------------

      subroutine del_comment (str)
      character str*80
      i1 = index (str,'!')
      i2 = index (str,'#')
      if (i1 .ne. 0) then
         str(i1:80) = ' '
      elseif (i2 .ne. 0) then
         str(i2:80) = ' '
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine move_first_element (str1,str2)

      character str1*80,str2*80

      str2 = ' '
      if (str1 .ne. ' ') then
         call strip(str1)
         i = nextbl(str1)
         str2 = str1(1:i)
         str1(1:i) = ' '
         if (str1 .ne. ' ') then
            call strip (str1)
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine upline(str80)
      character str80*80
      do 1 i = 1,80
         if (str80(i:i) .ge. 'a' .and. str80(i:i) .le. 'z') then
            str80(i:i) = char(ichar(str80(i:i))-ichar(' '))
         endif
    1 continue
      return
      end

c-----------------------------------------------------------------------

      subroutine strip(str80)
      character str80*80,temp*80
      integer firstch
      temp = str80(firstch(str80):80)
      str80 = temp
      return
      end

c-----------------------------------------------------------------------

      integer function nextbl(str80)
      character str80*80
      do 1 i = 1,80
         if (str80(i:i) .eq. ' ') then
            nextbl = i
            return
         endif
    1 continue
      nextbl = 81
      return
      end

c-----------------------------------------------------------------------

      integer function firstch(str80)
      character str80*80
      do 1 i = 1,80
         if (str80(i:i) .ne. ' ') then
            firstch = i
            return
         endif
    1 continue
      firstch = 81
      return
      end

c-----------------------------------------------------------------------

      integer function lastch(str80)
      character str80*80
      do 1 i = 80,1,-1
         if (str80(i:i) .ne. ' ') then
            lastch = i
            return
         endif
    1 continue
      lastch = 0
      return
      end

c-----------------------------------------------------------------------

      integer function lastc(str80)
      character str80*80
      lastc = lastch(str80)
      return
      end

c-----------------------------------------------------------------------

      subroutine blkstr(ifrom,ito,str80)
      character str80*80
      do 1 i = ifrom,ito
         str80(i:i) = ' '
    1 continue
      return
      end

c-----------------------------------------------------------------------

      logical function empty(str80)
      character str80*80
      if (str80 .eq. ' ') then
         empty = .true.
      else
         empty = .false.
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine gaussj (a,n,b,ising)
c
c --- This routine solves a linear equation system with the
c     Gauss-Jordan technique using full pivoting.
c     A is an input matrix of n*n elements, stored in an array of
c     dimension 6*6. B is an input vector containing the
c     right hand side vector, stored in an array of dimension 6.
c     A is replaced by its matrix inverse and B is replaced with the
c     corresponding solution vector.
c
c     Routine copied from "Numerical recipes" chapter 2, page 28
c
      parameter (nmax=6)
      dimension a(nmax,nmax),b(nmax),ipiv(nmax),indxr(nmax),indxc(nmax)

      do 11 j = 1,n
         ipiv(j) = 0
   11 continue
c
c --- set "ising" flag to indicate non-singular matrix.
c     any condition resulting in singular matrixes, sets "ising" to the
c     value of 1.
c
      ising = 0

      do 22 i = 1,n
         big = 0.0
         do 13 j = 1,n
            if (ipiv(j) .ne. 1) then
               do 12 k = 1,n
                  if (ipiv(k) .eq. 0) then
                     if (abs(a(j,k)) .ge. big) then
                        big = abs(a(j,k))
                        irow = j
                        icol = k
                     endif
                  elseif (ipiv(k) .gt. 1) then
c
c ------------------ singular matrix.
c
                     ising = 1
                     return
                  endif
   12          continue
            endif
   13    continue
         ipiv(icol) = ipiv(icol)+1
         if (irow .ne. icol) then
            do 14 l = 1,n
               dum = a(irow,l)
               a(irow,l) = a(icol,l)
               a(icol,l) = dum
   14       continue
            dum = b(irow)
            b(irow) = b(icol)
            b(icol) = dum
         endif
         indxr(i) = irow
         indxc(i) = icol
         if (a(icol,icol) .eq. 0.0) then
c
c --------- singular matrix.
c
            ising = 1
            return
         endif
         pivinv = 1.0/a(icol,icol)
         a(icol,icol) = 1.0
         do 16 l = 1,n
            a(icol,l) = a(icol,l)*pivinv
   16    continue
         b(icol) = b(icol)*pivinv
c
c ------ now reduce the rows, except for the pivot one.
c
         do 21 ll = 1,n
            if (ll .ne. icol) then
               dum = a(ll,icol)
               a(ll,icol) = 0.0
               do 18 l = 1,n
                  a(ll,l) = a(ll,l)-a(icol,l)*dum
   18          continue
               b(ll) = b(ll)-b(icol)*dum
            endif
   21    continue
   22 continue
c
c --- This is the end of the main loop over columns of the reduction.
c     It only remains to unscramble the solution in view of the column
c     interchanges. 
c
      do 24 l = n,1,-1
         if (indxr(l) .ne. indxc(l)) then
            do 23 k = 1,n
               dum = a(k,indxr(l))
               a(k,indxr(l)) = a(k,indxc(l))
               a(k,indxc(l)) = dum
   23       continue
         endif
   24 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine setlat (lukeys,ludisp,ilatt,str)
c
c --- this routine sets the lattice code to use.
c
      character answer*1,str*80

      if (str .eq. ' ') then
         write (ludisp,'(a)') 
     >   ' Available lattice-restrictions on I(hkl)'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' P, F, I, A, B, C or R.'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a,$)') ' Enter your choice here:'
         read  (lukeys,'(a)') answer
      else
         answer = str(1:1)
      endif

      if (answer .eq. 'p' .or. answer .eq. 'P') then
         ilatt = 1
      elseif (answer .eq. 'f' .or. answer .eq. 'F') then
         ilatt = 2
      elseif (answer .eq. 'i' .or. answer .eq. 'I') then
         ilatt = 3
      elseif (answer .eq. 'a' .or. answer .eq. 'A') then
         ilatt = 4
      elseif (answer .eq. 'b' .or. answer .eq. 'B') then
         ilatt = 5
      elseif (answer .eq. 'c' .or. answer .eq. 'C') then
         ilatt = 6
      elseif (answer .eq. 'r' .or. answer .eq. 'R') then
         ilatt = 7
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine open_log_file
     > (lukeys,ludisp,lulog,logfile_open,str,iok)
c
c --- this routine opens the log-file.
c
      character fname*80,str*80
      logical   logfile_open

      if (logfile_open) then
         write(ludisp,'(a)') ' Logfile allready open ...'
         write(ludisp,'(a)') 
     >   ' Close old logfile with CLOSE before opening a new one ...'
         iok = 0
         return
      endif

      if (str .eq. ' ') then
         write (ludisp,'(a,$)')
     >   ' Enter name of log-file:'
         read  (lukeys,'(a)') fname
      else
         fname = str
      endif

      if (fname .eq. ' ') then
         write(ludisp,'(a)') ' Blank filenames are not allowed ...'
         iok = 0
      else
         open(lulog,file=fname,status='unknown',iostat=istat)
         if (istat .ne. 0) then
            write (ludisp,'(a)') ' Sorry, cant open this file...'
            logfile_open = .false.
            iok = 0
         else
            logfile_open = .true.
            iok = 1
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine toglog(ilogfl,lukeys,ludisp,lulog,logfile_open,str)
c
c --- this routine toggles between logging or no logging of data written
c     to the screen.
c
      logical   logfile_open
      character str*80

      if (ilogfl .eq. 1) then
         ilogfl = 0
         write (ludisp,'(a)') ' Logging disabled.'
      else
         if (logfile_open) then
            ilogfl = 1
         else
            call open_log_file
     >           (lukeys,ludisp,lulog,logfile_open,str,iok)
            if (iok .eq. 1) then
               write (ludisp,'(a)') ' Logging enabled.'
               ilogfl = 1
            else
               ilogfl = 0
            endif
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine sethkl
c
c --- this routine enables the user to assign a specific HKL to a line.
c
      include 'puder.cmn'
      character answer*1,str80*80
c
      write (ludisp,'(a)')
     > ' This procedure sets HKL for individual lines.'
      write (ludisp,'(a)') ' Give line number 0 to exit.'
      write (ludisp,'(a)') ' '
    1 continue
      write (ludisp,'(a,$)') ' Give line-number .......:'
      read  (lukeys,'(a)') str80
      call get_int(str80,nummer,istat)
      if (istat .ne. 0) then
         return
      endif
      if (nummer .le. 0 .or. nummer .gt. nobs) then
         return
      endif
      twothe = todeg(2.0*sqrt(cl2q*qobs(nummer)))
      write (ludisp,'(a,f9.6,f9.3)')
     >' Qobs and 2Theta ......:',qobs(nummer),twothe
      write (ludisp,'(a,$)') ' Give HKL for this line .:'
      read  (lukeys,*,iostat=istat) i1,i2,i3
      if (istat .eq. 0) then
         ih(nummer) = i1
         ik(nummer) = i2
         il(nummer) = i3
         nokay(nummer) = 1
         qcalc(nummer) =
     >   get_qcalc(i1,i2,i3,g11,g22,g33,g12,g13,g23)
         write(ludisp,'(a,$)') ' Lock it ? <Y/N[def]> ...:'
         read(lukeys,'(a)') answer
         if (answer .eq. 'y' .or. answer .eq. 'Y') then
            ilock(nummer) = 1
         else
            ilock(nummer) = 0
         endif
         write (ludisp,'(a)') ' '
         goto 1
      else
         return
      endif
c
      end

c-----------------------------------------------------------------------

      function get_qcalc(h,k,l,g11,g22,g33,g12,g13,g23)
      integer h,k,l
      get_qcalc = real(h*h)*g11 + real(k*k)*g22 + real(l*l)*g33 + 
     >            real(h*k)*g12 + real(h*l)*g13 + real(k*l)*g23
      return
      end

c-----------------------------------------------------------------------

      subroutine lockli(str80)
c
c --- this routine enables the user to lock the HKL indices of a line.
c
      include 'puder.cmn'
      character str80*80

      if (str80 .eq. ' ') then
         write (ludisp,'(a)') ' Missing parameter ?'
         return
      endif

      call upline (str80)
      if (str80 .eq. 'ALL') then
         do i = 1,nobs
            qcalc(i) = 
     >      get_qcalc(ih(i),ik(i),il(i),g11,g22,g33,g12,g13,g23)
            ilock(i) = 1
         enddo
         return

      else
c
c ------ lock some specific line.
c
         call get_int(str80,nummer,istat)
         if (istat .ne. 0) then
            return
         endif
         if (nummer .le. 0 .or. nummer .gt. nobs) then
            return
         endif

         if (ilock(nummer) .eq. 1) then
            write(ludisp,'(a)')
     >      ' This line has allready locked indexes!'
         else 
            qcalc(nummer) = 
     >      get_qcalc(ih(nummer),ik(nummer),il(nummer),
     >                g11,g22,g33,g12,g13,g23)
            ilock(nummer) = 1
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine unlock(str80)
c
c --- this routine enables the user to unlock the HKL indices of a line.
c
      include 'puder.cmn'
      character str80*80

      if (str80 .eq. ' ') then
         write (ludisp,'(a)') ' Missing parameter ?'
         return
      endif

      call upline(str80)
      if (str80 .eq. 'ALL') then
         do i = 1,nobs
            ilock(i) = 0
         enddo
         return

      else
c
c ------ unlock some specific line.
c
         call get_int(str80,nummer,istat)
         if (istat .ne. 0) then
            return
         endif
         if (nummer .le. 0 .or. nummer .gt. nobs) then
            return
         endif
         if (ilock(nummer) .eq. 0) then
            write(ludisp,'(a)') ' This line is not locked ...'
         else
            ilock(nummer) = 0
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine setsys (lukeys,ludisp,isyst,str)
c
c --- this routine sets the crystal system.
c
      character str*80

      if (str .eq. ' ') then
         write (ludisp,'(9(a,/),a)')
     >   ' These are the available crystal systems.',
     >   ' ',
     >   ' 1) Cubic',
     >   ' 2) Trigonal',
     >   ' 3) Tetragonal',
     >   ' 4) Hexagonal',
     >   ' 5) Orthorhombic',
     >   ' 6) Monoclinic',
     >   ' 7) Triclinic',
     >   ' '

         write (ludisp,'(a,$)') ' Choose one the above systems:'

         read  (lukeys,'(a)') str
         call get_int(str,nummer,istat)
         if (istat .ne. 0) then
            return
         endif

      else
         call upline (str)
         if (str .eq. 'CUBIC' .or.
     >       str .eq. 'CUBI' .or.
     >       str .eq. 'CUB' .or.
     >       str .eq. 'CU' .or.
     >       str .eq. '1' ) then
            nummer = 1

         elseif (str .eq. 'TRIGONAL' .or.
     >           str .eq. 'TRIGONA' .or.
     >           str .eq. 'TRIGON' .or.
     >           str .eq. 'TRIGO' .or.
     >           str .eq. 'TRIG' .or. 
     >           str .eq. '2' ) then
            nummer = 2

         elseif (str .eq. 'TETRAGONAL' .or.
     >           str .eq. 'TETRAGONA' .or.
     >           str .eq. 'TETRAGON' .or.
     >           str .eq. 'TETRAGO' .or.
     >           str .eq. 'TETRAG' .or.
     >           str .eq. 'TETRA' .or.
     >           str .eq. 'TETR' .or.
     >           str .eq. 'TET' .or.
     >           str .eq. 'TE' .or.
     >           str .eq. '3' ) then
            nummer = 3

         elseif (str .eq. 'HEXAGONAL' .or.
     >           str .eq. 'HEXAGONA' .or.
     >           str .eq. 'HEXAGON' .or.
     >           str .eq. 'HEXAGO' .or.
     >           str .eq. 'HEXAG' .or.
     >           str .eq. 'HEXA' .or.
     >           str .eq. 'HEX' .or.
     >           str .eq. 'HE' .or.
     >           str .eq. '4' ) then
            nummer = 4

         elseif (str .eq. 'ORTORHOMBIC' .or.
     >           str .eq. 'ORTORHOMBI' .or.
     >           str .eq. 'ORTORHOMB' .or.
     >           str .eq. 'ORTORHOM' .or.
     >           str .eq. 'ORTORHO' .or.
     >           str .eq. 'ORTORH' .or.
     >           str .eq. 'ORTOR' .or.
     >           str .eq. 'ORTO' .or.
     >           str .eq. 'ORT' .or.
     >           str .eq. 'OR' .or.
     >           str .eq. '5' ) then
            nummer = 5
 

         elseif (str .eq. 'MONOCLINIC' .or.
     >           str .eq. 'MONOCLINI' .or.
     >           str .eq. 'MONOCLIN' .or.
     >           str .eq. 'MONOCLI' .or.
     >           str .eq. 'MONOCL' .or.
     >           str .eq. 'MONOC' .or.
     >           str .eq. 'MONO' .or.
     >           str .eq. 'MON' .or.
     >           str .eq. 'MO' .or.
     >           str .eq. '6' ) then
            nummer = 6

         elseif (str .eq. 'TRICLINIC' .or.
     >           str .eq. 'TRICLINI' .or.
     >           str .eq. 'TRICLIN' .or.
     >           str .eq. 'TRICLI' .or.
     >           str .eq. 'TRICL' .or.
     >           str .eq. 'TRIC' .or.
     >           str .eq. '7' ) then
            nummer = 7
 
        else
            write (ludisp,'(a)') ' '
            write (ludisp,'(a)') 'Unknown crystal system.'

         endif
      endif
     
      if (nummer .ge. 1 .and. nummer .le. 7) then
         isyst = nummer
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine setuse (lukeys,ludisp,n,str80)
c
c --- this routine sets the number of allowed indexes for a line, if it
c     should be accepted in the refinement procedure.
c
      character str80*80

      if (str80 .eq. ' ') then
         write(ludisp,'(a,a)')
     >   ' Give number of allowed HKL/line, to be accepted',
     >   ' in the refinement procedure.'
         write(ludisp,'(a)') ' '
         write(ludisp,'(a,i5)') ' Current value is:',n
         write(ludisp,'(a)') ' '
         write(ludisp,'(a,$)') 
     >   ' Enter new value !  (CR = no change) ...:'

         read  (lukeys,'(a)') str80
      endif

      call get_int(str80,nummer,istat)
      if (istat .ne. 0) then
         return
      endif
      if (nummer .ge. 1) then
         n = nummer
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine setweg (str80)
c
c --- this routine enables the user to change the weighting scheme.
c
      include 'puder.cmn'

      character str80*80

      if (str80 .eq. ' ') then
         write(ludisp,'(a)')
     >   ' These are the available weighting schemes.'
         write(ludisp,'(a)') ' '
         write(ludisp,'(7(a,/),a)') 
     >   '     1) Unit weights.',
     >   '     2) Hess weights.',
     >   '     3) Individual weights for each line.',
     >   '     4) Proportional weights for each line.',
     >   '        (Weight = 1 / (number of index-combinations)).',
     >   '     5) Weight = 1 / sine(theta)**2.0',
     >   '     6) Weight = 1 / sine(theta)**3.0',
     >   ' '

         write(ludisp,'(a,$)') ' Choose one of them (!) :'

         read  (lukeys,'(a)') str80
         call get_int(str80,nummer,istat)
         if (istat .ne. 0) then
            return
         endif
         if (nummer .lt. 1 .or. nummer .gt. 6) then
            return
         endif
      else
         call upline (str80)
         if (str80 .eq. 'UNIT' .or.
     >       str80 .eq. 'UNI' .or.
     >       str80 .eq. 'UN' .or.
     >       str80 .eq. 'U' .or.
     >       str80 .eq. '1' ) then
            nummer = 1

         elseif (str80 .eq. 'HESS' .or.
     >           str80 .eq. 'HES' .or.
     >           str80 .eq. 'HE' .or.
     >           str80 .eq. 'H' .or.
     >           str80 .eq. '2' ) then
            nummer = 2

         elseif (str80 .eq. 'INDIVIDUAL' .or.
     >           str80 .eq. 'INDIVIDUA' .or.
     >           str80 .eq. 'INDIVIDU' .or.
     >           str80 .eq. 'INDIVID' .or.
     >           str80 .eq. 'INDIVI' .or.
     >           str80 .eq. 'INDIV' .or.
     >           str80 .eq. 'INDI' .or.
     >           str80 .eq. 'IND' .or.
     >           str80 .eq. 'IN' .or.
     >           str80 .eq. 'I' .or.
     >           str80 .eq. '3' ) then
            nummer = 3

         elseif (str80 .eq. 'PROPORTIONAL' .or.
     >           str80 .eq. 'PROPORTIONA' .or.
     >           str80 .eq. 'PROPORTION' .or.
     >           str80 .eq. 'PROPORTIO' .or.
     >           str80 .eq. 'PROPORTI' .or.
     >           str80 .eq. 'PROPORT' .or.
     >           str80 .eq. 'PROPOR' .or.
     >           str80 .eq. 'PROPO' .or.
     >           str80 .eq. 'PROP' .or.
     >           str80 .eq. 'PRO' .or.
     >           str80 .eq. 'PR' .or.
     >           str80 .eq. 'P' .or.
     >           str80 .eq. '5' ) then
            nummer = 4

         elseif (str80 .eq. '5') then
            nummer = 5

         elseif (str80 .eq. '6') then
            nummer = 6

         endif
      endif
c
c --- now actually set the type of weight-scheme to be used in 
c     the refinement.
c
      if (nummer .eq. 1) then
         iwgtyp = 1
      elseif (nummer .eq. 2) then
         iwgtyp = 2
      elseif (nummer .eq. 3) then
         iwgtyp = 3
      elseif (nummer .eq. 4) then
         iwgtyp = 4
      elseif (nummer .eq. 5) then
         iwgtyp = 5
      elseif (nummer .eq. 6) then
         iwgtyp = 6
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine shstat(idev)
c
c --- this routine shows some status values.
c
      include 'puder.cmn'

      character*12 syst_name(7)
      character*1  latt_name(7)

      data syst_name(1) /'       Cubic'/
      data syst_name(2) /'    Trigonal'/
      data syst_name(3) /'  Tetragonal'/
      data syst_name(4) /'   Hexagonal'/
      data syst_name(5) /'Orthorhombic'/
      data syst_name(6) /'  Monoclinic'/
      data syst_name(7) /'   Triclinic'/

      data latt_name /'P','F','I','A','B','C','R'/

      if (wave .ge. 1.0) then
         write(idev,'(a,f12.6)') ' Wavelength .........:',wave
      else
         write(idev,'(a,f12.7)') ' Wavelength .........:',wave
      endif
      write(idev,'(a,f12.4)') ' delta (2theta) .....:',delta_twotheta
      write(idev,'(a,a)') ' Crystal system .....:',syst_name(isyst)
      write(idev,'(a,a)')
     >' Lattice centering ..:           ',latt_name(ilatt)
      if (ilogfl .eq. 1) then
         write(idev,'(a)')    ' Logging ............:          ON'
      else
         write(idev,'(a)')    ' Logging ............:         OFF'
      endif
      write(idev,'(a,i12)')   ' Number of obs.-lines:',nobs
      write(idev,'(a,i12)')   ' Allowed HKL / line .:',linuse

      return
      end

c-----------------------------------------------------------------------

      subroutine calcul
c
c --- this routine calculates some different things.
c
      include 'puder.cmn'

      character answer*1, fname*80
      integer   h,k,l

      write(ludisp,'(a)')
     > ' What do you want to calculate ?'
      write(ludisp,'(a)')
     > ' '
      write(ludisp,'(a)')
     > ' 1) The position of a line.'
      write(ludisp,'(a)')
     > ' 2) Number of possible lines up to some line.'
      write(ludisp,'(a)')
     > ' 3) Number of possible lines up to some 2theta limit.'
      write(ludisp,'(a)')
     > ' 4) All possible lines up to some line.'
      write(ludisp,'(a)')
     > ' 5) All possible lines up to some 2theta limit.'
      write(ludisp,'(a)')
     > ' '
      write(ludisp,'(a,$)')
     > ' Enter your choice here:'

      read(lukeys,'(a)') answer
c
c --- calculate the position of one specific reflexion.
c
      if (answer .eq. '1') then
         write(ludisp,'(a)') ' '
         write(ludisp,'(a,$)') ' Enter the HKL-indices:'
         read (lukeys,*,iostat=istat) h,k,l
         if (istat .ne. 0) then
            write(ludisp,'(a)') 
     >      ' IO-Error encountered when reading the HKL-value.'
            return
         endif
         q = h*h*g11+k*k*g22+l*l*g33+h*k*g12+h*l*g13+k*l*g23
         ssq = q_2_ssq(q,wave)
         if (ssq .gt. 1.0) then
            write(ludisp,'(a,f10.2,a)')
     >      ' Sine quare theta = ',ssq,
     >      ' which would give a complex angle.'
         else
            twothe = q_2_twoth(q,wave)
            write(ludisp,'(3i4,2f9.6,f9.3)') h,k,l,q,ssq,twothe
         endif
c
c --- Number of possible lines up to some line,
c
      elseif (answer .eq. '2') then
         write(ludisp,'(a)') ' '
         write(ludisp,'(a,$)') ' Give the observed-line number:'
         read (lukeys,*,iostat=istat) nused
         if (istat .ne. 0) then
            write(ludisp,'(a)') 
     >      ' IO-Error encountered when reading line number.'
            return
         endif
         if (nused .ge. 1 .and. nused .le. nobs) then
            nposs = ntheor(0,0,lukeys,ludisp,lu_theo_hkl,isyst,
     >                     qobs(nused)+qdel(nused),
     >                     g11,g22,g33,g12,g13,g23,wave)
            write(ludisp,'(a,i10)') ' Number of possible lines = ',nposs
         endif
c
c --- Number of possible lines up to some 2theta limit.
c
      elseif (answer .eq. '3') then
         write(ludisp,'(a)') ' '
         write(ludisp,'(a,$)') ' Give upper 2theta limit:'
         read (lukeys,*,iostat=istat) twothe
         if (istat .ne. 0) then
            write(ludisp,'(a)') 
     >      ' IO-Error encountered when reading upper 2theta limit.'
            return
         endif
         ssqlim = sqr(sin(torad(0.5*twothe)))
         qlim = ssq_2_q(ssqlim,wave)
         if (nused .ge. 1 .and. nused .le. nobs) then
            nposs = ntheor(0,0,lukeys,ludisp,lu_theo_hkl,isyst,qlim,
     >                     g11,g22,g33,g12,g13,g23,wave)
            write(ludisp,'(a,i10)') ' Number of possible lines = ',nposs
         endif
c
c --- calculate all possible lines up to some line.
c     or calculate all possible lines up to some 2theta limit.
c
      elseif (answer .eq. '4' .or. answer .eq. '5') then
         if (answer .eq. '4') then
            write(ludisp,'(a,$)') ' Give the observed-line number:'
            read (lukeys,*,iostat=istat) nused
            if (istat .ne. 0) then
               write(ludisp,'(a)') 
     >         ' IO-Error encountered when reading line-number.'
               return
            endif
            if (nused .lt. 1 .and. nused .gt. nobs) then
               return
            endif
            qlim = qobs(nused)+qdel(nused)

         elseif (answer .eq. '5') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a,$)') ' Give upper 2theta limit:'
            read (lukeys,*,iostat=istat) twothe
            if (istat .ne. 0) then
               write(ludisp,'(a)') 
     >         ' IO-Error encountered when reading upper 2theta-limit.'
               return
            endif
            qlim = twoth_2_q(twothe,wave)
         endif

         write(ludisp,'(a,$)') ' Display(D) or file(F) ? '
         read (lukeys,'(a)') answer
         if (answer .eq. 'd' .or. answer .eq. 'D') then
            ifil = 0
         elseif (answer .eq. 'f' .or. answer .eq. 'F') then
            write(ludisp,'(a,$)')
     >      ' Enter filename for theoretical line positions: '
            read (lukeys,'(a)') fname
            open(lu_theo_hkl,file=fname,iostat=istat)
            if (istat .ne. 0) then
               return
            endif
            ifil = 1
         else
            return
         endif         
         ndum = ntheor(1,ifil,lukeys,ludisp,lu_theo_hkl,isyst,qlim,
     >                 g11,g22,g33,g12,g13,g23,wave)
         write(ludisp,'(a,i8)') ' Number of possible lines = ',ndum
         if (ifil .eq. 1) then
            close (lu_theo_hkl)
         endif

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine celtra
c
c --- this routine transforms the cell.
c
      include 'puder.cmn'

      write(ludisp,'(a,$)') ' Give factor to multiply A with:'
      read (lukeys,*) facta
      a = a*facta
      write(ludisp,'(a,$)') ' Give factor to multiply B with:'
      read (lukeys,*) factb
      b = b*factb
      write(ludisp,'(a,$)') ' Give factor to multiply C with:'
      read (lukeys,*) factc
      c = c*factc

      siga = 0.0
      sigb = 0.0
      sigc = 0.0
      sigal = 0.0
      sigbe = 0.0
      sigga = 0.0

      call systry (1)
      call getrec
      call getgij


      return
      end

c-----------------------------------------------------------------------

      subroutine rdcell (str)
c
c --- this routine reads the cell data.
c
      include 'puder.cmn'

      character answer*1, str*80, curr*80

      iok = 0
      if (str .eq. ' ') then
c
c ------ read cell parameters from keyboard.
c
         answer = '1'
    1    continue
         if (answer.eq.'1' .or. answer.eq.'2' .or.
     >       answer.eq.'3' .or. answer.eq.'4' .or.
     >       answer.eq.'5' .or. answer.eq.'6') then
            write(ludisp,'(a)') ' These are the current cellparameters.'
            write(ludisp,'(a)') ' '
            write(ludisp,'(a,f10.5)') '   1) a ......: ',a
            write(ludisp,'(a,f10.5)') '   2) b ......: ',b
            write(ludisp,'(a,f10.5)') '   3) c ......: ',c
            write(ludisp,'(a,f10.5)') '   4) alfa ...: ',al
            write(ludisp,'(a,f10.5)') '   5) beta ...: ',be
            write(ludisp,'(a,f10.5)') '   6) gamma ..: ',ga
            write(ludisp,'(a)') ' '
            write(ludisp,'(a,$)') ' Enter your choice (1..6) here:'
            read (lukeys,'(a)') answer
      
            if (answer .eq. '1') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new a :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     a = temp
                     siga = 0.0
                  endif
               endif

            elseif (answer .eq. '2') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new b :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     b = temp
                     sigb = 0.0
                  endif
               endif

            elseif (answer .eq. '3') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new c :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     c = temp
                     sigc = 0.0
                  endif
               endif

            elseif (answer .eq. '4') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new alfa :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     al = temp
                     sigal = 0.0
                  endif
               endif

            elseif (answer .eq. '5') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new beta :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     be = temp
                     sigbe = 0.0
                  endif
               endif

            elseif (answer .eq. '6') then
               write(ludisp,'(a)') ' '
               write(ludisp,'(a,$)') ' Give new gamma :'
               read (lukeys,'(a)') curr
               if (curr .ne. ' ') then
                  call strip (curr)
                  call get_real(curr,temp,istat)
                  if (istat .eq. 0) then
                     ga = temp
                     sigga = 0.0
                  endif
               endif
            endif
            goto 1
         endif
         call systry (1)
         iok = 1
      else
c
c ------ read cell parameters from parameters string.
c         
         istata  = 1
         istatb  = 1
         istatc  = 1
         istatal = 1
         istatbe = 1
         istatga = 1
         call move_first_element (str, curr)
         if (curr .ne. ' ') then
            call get_real(curr,tempa,istata)
         endif
         if (str .ne. ' ') then
            call move_first_element (str, curr)
            if (curr .ne. ' ') then
               call get_real(curr,tempb,istatb)
            endif
         endif
         if (str .ne. ' ') then
            call move_first_element (str, curr)
            if (curr .ne. ' ') then
               call get_real(curr,tempc,istatc)
            endif
         endif
         if (str .ne. ' ') then
            call move_first_element (str, curr)
            if (curr .ne. ' ') then
               call get_real(curr,tempal,istatal)
            endif
         endif
         if (str .ne. ' ') then
            call move_first_element (str, curr)
            if (curr .ne. ' ') then
               call get_real(curr,tempbe,istatbe)
            endif
         endif
         if (str .ne. ' ') then
            call move_first_element (str, curr)
            if (curr .ne. ' ') then
               call get_real(curr,tempga,istatga)
            endif
         endif
c
c ------ if six parameters have been read ok, then set the
c        cell-parameters.
c
         if (istata  .eq. 0 .and. istatb  .eq. 0 .and. 
     >       istatc  .eq. 0 .and. istatal .eq. 0 .and. 
     >       istatbe .eq. 0 .and. istatga .eq. 0 ) then
            a = tempa
            siga = 0.0
            b = tempb
            sigb = 0.0
            c = tempc
            sigc = 0.0
            al = tempal
            sigal = 0.0
            be = tempbe
            sigbe = 0.0
            ga = tempga
            sigga = 0.0

            call systry (0)
            iok = 1

         else
            write (ludisp,'(a)') ' '
            write (ludisp,'(a)') 
     >      ' Error encountered during decoding of CELL instruction.'

         endif

      endif

      if (iok .eq. 1) then
         call getrec
         call getgij
     
         write (ludisp,'(a)') ' '
         write (ludisp,'(a,3f10.5,3f10.4)')
     >   ' Real cell .....:',a,b,c,al,be,ga
         write (ludisp,'(a,3f10.6,3f10.4)')
     >   ' Rec. cell .....:',ast,bst,cst,alst,best,gast
         write (ludisp,'(a,6f10.7)')
     >   ' Gij parameters :',g11,g22,g33,g12,g13,g23
         write (ludisp,'(a)') ' '
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine systry (ikey)
c
c --- this routine tries to establish the crystal system from
c     relations among the cell parameters.
c
c    ikey = 0 means don't ask whether ok or not.
c    ikey = 1 means do ask whether ok or not.
c
      include 'puder.cmn'

      character answer*1

      write (ludisp,'(a)') ' '
    1 continue
      if (abs(a/b-1.0) .lt. 0.00001 .and.
     >    abs(a/c-1.0) .lt. 0.00001 .and.
     >    abs(al-90.0) .lt. 0.00001 .and.
     >    abs(be-90.0) .lt. 0.00001 .and.
     >    abs(ga-90.0) .lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the CUBIC crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 1
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      elseif (abs(a/b-1.0)   .lt. 0.00001 .and.
     >        abs(a/c-1.0)   .lt. 0.00001 .and.
     >        abs(al/be-1.0) .lt. 0.00001 .and.
     >        abs(al/ga-1.0) .lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the TRIGONAL crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 2
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      elseif (abs(a/b-1.0) .lt. 0.00001 .and.
     >        abs(al-90.0) .lt. 0.00001 .and.
     >        abs(be-90.0) .lt. 0.00001 .and.
     >        abs(ga-90.0) .lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the TETRAGONAL crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 3
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      elseif (abs(a/b-1.0) .lt. 0.00001 .and.
     >        abs(al-90.0) .lt. 0.00001 .and.
     >        abs(be-90.0) .lt. 0.00001 .and.
     >        abs(ga-120.0).lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the HEXAGONAL crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 4
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      elseif (abs(al-90.0) .lt. 0.00001 .and.
     >        abs(be-90.0) .lt. 0.00001 .and.
     >        abs(ga-90.0) .lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the ORTORHOMBIC crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 5
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      elseif (abs(al-90.0) .lt. 0.00001 .and.
     >        abs(ga-90.0) .lt. 0.00001 ) then
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the MONOCLINIC crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 6
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      else
         if (ikey .eq. 0) then
            answer = 'Y'
         else
            write (ludisp,'(a,$)')
     >      ' Is this the TRICLINIC crystal system ?  <Y[Def]/N> '
            read  (lukeys,'(a)') answer
         endif
         if (answer.eq.'Y' .or. answer.eq.'y' .or. answer.eq.' ') then
            isyst = 7
         elseif (answer.ne.'N' .and. answer.ne.'n') then
            write(ludisp,'(a)') ' '
            write(ludisp,'(a)') ' You MUST answer Y,N or only return !'
            write(ludisp,'(a)') ' '
            goto 1
         endif         

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine getrec
c
c --- this routine computes the reciprocal cell parameters.
c
      include 'puder.cmn'

      if (isyst.ge.1 .and. isyst.le.7) then

         sigast = 0.0
         sigbst = 0.0
         sigcst = 0.0
         sigalst = 0.0
         sigbest = 0.0
         siggast = 0.0

         v = vol (a,b,c,torad(al),torad(be),torad(ga))

         ast = b*c*sin(torad(al)) / v   
         bst = a*c*sin(torad(be)) / v
         cst = a*b*sin(torad(ga)) / v

         alst = acos ( (cos(torad(be))*cos(torad(ga))-cos(torad(al))) /
     >            (sin(torad(be))*sin(torad(ga))) )
         best = acos ( (cos(torad(al))*cos(torad(ga))-cos(torad(be))) /
     >            (sin(torad(al))*sin(torad(ga))) )
         gast = acos ( (cos(torad(al))*cos(torad(be))-cos(torad(ga))) /
     >            (sin(torad(al))*sin(torad(be))) )
         alst=todeg(alst)
         best=todeg(best)
         gast=todeg(gast)

      else
         write (ludisp,'(a)') ' Unknown crystal system.'
      endif

      return
      end

c-----------------------------------------------------------------------

      function vol (a,b,c,al,be,ga)
c
c --- this function returns the cell volume ...
c
      vol = a*b*c * sqrt (1.0-sqr(cos(al))-sqr(cos(be))-sqr(cos(ga))+
     >      2.0*cos(al)*cos(be)*cos(ga) )
c
      return
      end

c-----------------------------------------------------------------------

      subroutine getgij
c
c --- this routine computes the reciprokal metric tensor parameters
c     from the reciprokal cell constants.
c
      include 'puder.cmn'

      g11 = ast*ast
      g22 = bst*bst
      g33 = cst*cst
      g12 = 2.0*ast*bst*cos(torad(gast))
      g13 = 2.0*ast*cst*cos(torad(best))
      g23 = 2.0*bst*cst*cos(torad(alst))

      return
      end

c-----------------------------------------------------------------------

      subroutine merits (str80)
c
c --- this routine computes some different figure of merits,
c     from the indexed pattern.
c
      include 'puder.cmn'

      character str80*80
c
c --- decide how many lines that should be used in the FOM-calculations.
c
      if (str80 .eq. ' ') then
         nmerit = 20
      else
         call get_int(str80,nmerit,istat)
         if (istat .ne. 0) then
            return
         endif
      endif
c
c --- compute the different figures of merit for the first NMERIT lines.
c
      nused = min(nobs,nmerit)
      qlimit = qobs(nused)+qdel(nused)
      nposs = ntheor(0,0,lukeys,ludisp,lu_theo_hkl,isyst,qlimit,
     >               g11,g22,g33,g12,g13,g23,wave)
      if (nposs .le. 0) then
         write(ludisp,'(a)')
     >   ' Nposs = 0, i.e. no theoretical lines possible, strange...(?)'
         write(ludisp,'(a)')
     >   ' No calculation of Figures of Merit, since Nposs is needed.'
         return
      endif
c
c --- Computes the delta-q and delta-2theta arrays...
c
      call get_del_arr_for_merits
c
c --- Sum the delta-q and the delta-2theta for the first NUSED lines.
c
      esum = 0.0
      tsum = 0.0
      ncalc = 0
      do i = 1,nused
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            esum = esum+smdlsq(i)
            tsum = tsum+smdl2t(i)
            ncalc = ncalc+nokay(i)
         endif
      enddo

      if (ncalc .lt. 1) then
         write(ludisp,'(a)') ' No calculated lines !'
         write(ludisp,'(a)') 
     >   ' It is not possible to calculate any FOMs.'
         return
      endif

      if (iverb .eq. 1) then
         write(ludisp,'(a,f10.6)') ' SUM(delta-q) ....: ',esum
         write(ludisp,'(a,f10.6)') ' SUM(delta-2th) ..: ',tsum
         write(ludisp,'(a,i10)')   ' NUSED ...........: ',nused
         write(ludisp,'(a,i10)')   ' NCALC ...........: ',ncalc
         write(ludisp,'(a,f10.6)') ' QOBS(nused) .....: ',qobs(nused)
         write(ludisp,'(a,i10)')   ' NPOSS ...........: ',nposs
      endif

      errq = esum/real(ncalc)
      e20 = qobs(nused)*0.5/(nposs*errq)
      err2th = tsum/real(ncalc)
      f20 = nused/(nposs*todeg(err2th))
c
c --- write the desired figure of merits.
c
      if (nused .le. 99) then
         write (ludisp,'(a,i2,a,f8.1,a,f10.8)') 
     >   ' M(',nused,') = ',e20,' Average epsilon =',errq
         write (ludisp,'(a,i2,a,f8.1,a,f8.6,i4,a)')
     >   ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         if (ilogfl .eq. 1) then
            write (lulog,'(a,i2,a,f8.1,a,f10.8)') 
     >      ' M(',nused,') = ',e20,' Average epsilon =',errq
            write (lulog,'(a,i2,a,f8.1,a,f8.6,i4,a)')
     >      ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         endif

      elseif (nused .le. 999) then
         write (ludisp,'(a,i3,a,f8.1,a,f10.8)') 
     >   ' M(',nused,') = ',e20,' Average epsilon =',errq
         write (ludisp,'(a,i3,a,f8.1,a,f8.6,i5,a)')
     >   ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         if (ilogfl .eq. 1) then
            write (lulog,'(a,i3,a,f8.1,a,f10.8)') 
     >      ' M(',nused,') = ',e20,' Average epsilon =',errq
            write (lulog,'(a,i3,a,f8.1,a,f8.6,i5,a)')
     >      ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         endif

      elseif (nused .le. 9999) then
         write (ludisp,'(a,i4,a,f8.1,a,f10.8)') 
     >   ' M(',nused,') = ',e20,' Average epsilon =',errq
         write (ludisp,'(a,i4,a,f8.1,a,f8.6,i6,a)')
     >   ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         if (ilogfl .eq. 1) then
            write (lulog,'(a,i4,a,f8.1,a,f10.8)') 
     >      ' M(',nused,') = ',e20,' Average epsilon =',errq
            write (lulog,'(a,i4,a,f8.1,a,f8.6,i6,a)')
     >      ' F(',nused,') = ',f20,' (',todeg(err2th),nposs,')'
         endif

      else
         write (ludisp,'(a)')
     >   ' You have a lot of lines (!!!!), redimension the program !'

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine get_del_arr_for_merits
c
c --- this routine computes the delta-q and delta-2theta arrays...
c
      include 'puder.cmn'
c
c --- set the error limits for the indexing to be accepted.
c
      call set_error_limits
c
c --- Choose crystal system and compute the 
c     delta-q and delta-2theta arrays...
c
      if (isyst .eq. 1) then
         call merit_cub
      elseif (isyst .eq. 2) then
         call merit_trig
      elseif (isyst .eq. 3) then
         call merit_tet
      elseif (isyst .eq. 4) then
         call merit_hex
      elseif (isyst .eq. 5) then
         call merit_ort
      elseif (isyst .eq. 6) then
         call merit_mon
      elseif (isyst .eq. 7) then
         call merit_tric
      else
         write (ludisp,'(a)')
     >   ' Unknown crystal system, no merit calculation done ...'
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_cub
c
c --- This routine computes the elements of the delta-q and
c     delta-2theta arrays, for a cubic cell.
c
      include 'puder.cmn'

      integer h,k,l,h2k2l2,old_h2k2l2
      logical extinct
      integer hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(sqrt((qobs(i)+qdel(i))/g11))
            old_h2k2l2 = 0
            do h = 0,hend
               kend = h
               do k = 0,kend
                  lend = k
                  do l = 0,lend
                     if (.not.extinct(h,k,l)) then		
                        h2k2l2 = h*h+k*k+l*l
                        qtest = (h2k2l2)*g11
                        absdel = abs(qtest-qobs(i))
                        if (absdel .lt. qdel(i) .and.
     >                      h2k2l2 .ne. old_h2k2l2) then
                           old_h2k2l2 = h2k2l2
                           smdlsq(i) = smdlsq(i) + abs(qtest-qobs(i))
                           smdl2t(i) = smdl2t(i) + abs(2.0 * 
     >                     (asin(sqrt(qtest)) - asin(sqrt(qobs(i)))))
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_trig
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for a tetragonal cell.
c
      include 'puder.cmn'

      integer h,k,l,h2k2,l2,old_h2k2,old_l2
      logical extinct
      integer hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(sqrt((qobs(i)+qdel(i))/g11))
            old_h2k2 = 0
            old_l2 = 0
            do h = 0,hend
               kend = h
               do k = 0,kend
                  rest = (qobs(i)+qdel(i)-real(h*h+k*k)*g11) / g33
                  if (rest .gt. 0.0) then
                     lend = nint(sqrt(rest))
                  else
                     lend = 0
                  endif
                  do l = 0,lend
                     if (.not.extinct(h,k,l)) then		
                        h2k2 = h*h+k*k
                        l2 = l*l
                        qtest = real(h2k2)*g11+real(l2)*g33
                        absdel = abs(qtest-qobs(i))
                        if (h2k2.ne.old_h2k2 .or. l2.ne.old_l2) then
                           if (absdel .lt. qdel(i)) then
                              old_h2k2 = h2k2
                              old_l2 = l2
                              smdlsq(i) = smdlsq(i) +
     >                        abs(qtest-qobs(i))
                              smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                        (asin(sqrt(qtest)) - 
     >                        asin(sqrt(qobs(i)))))
                           endif
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_tet
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for a tetragonal cell.
c
      include 'puder.cmn'

      integer h,k,l,h2k2,l2,old_h2k2,old_l2
      logical extinct
      integer hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(sqrt((qobs(i)+qdel(i))/g11))
            old_h2k2 = 0
            old_l2 = 0
            do h = 0,hend
               kend = h
               do k = 0,kend
                  rest = (qobs(i)+qdel(i)-real(h*h+k*k)*g11) / g33
                  if (rest .gt. 0.0) then
                     lend = nint(sqrt(rest))
                  else
                     lend = 0
                  endif
                  do l = 0,lend
                     if (.not.extinct(h,k,l)) then		
                        h2k2 = h*h+k*k
                        l2 = l*l
                        qtest = real(h2k2)*g11+real(l2)*g33
                        absdel = abs(qtest-qobs(i))
                        if (h2k2.ne.old_h2k2 .or. l2.ne.old_l2) then
                           if (absdel .lt. qdel(i)) then
                              old_h2k2 = h2k2
                              old_l2 = l2
                              smdlsq(i) = smdlsq(i) +
     >                        abs(qtest-qobs(i))
                              smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                        (asin(sqrt(qtest)) - 
     >                        asin(sqrt(qobs(i)))))
                           endif
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_hex
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for a primitive hexagonal cell.
c
      include 'puder.cmn'

      integer h,k,l,h2hkk2,l2,old_h2hkk2,old_l2
      logical extinct
      integer hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(sqrt((qobs(i)+qdel(i))/g11))
            old_h2hkk2 = 0
            old_l2 = 0
            do h = 0,hend
               kend = h
               do k = -h,kend
                  rest = (qobs(i)+qdel(i)-(h*h+h*k+k*k)*g11) / g33
                  if (rest .gt. 0.0) then
                     lend = nint(sqrt(rest))
                  else
                     lend = 0
                  endif
                  do l = 0,lend
                     if (.not.extinct(h,k,l)) then		
                        h2hkk2 = h*h+h*k+k*k
                        l2 = l*l
                        qtest = real(h2hkk2)*g11+real(l2)*g33
                        absdel = abs(qtest-qobs(i))
                        if (h2hkk2.ne.old_h2hkk2 .or.
     >                      l2.ne.old_l2 ) then
                           if (absdel .lt. qdel(i)) then
                              old_h2hkk2 = h2hkk2
                              old_l2 = l2
                              smdlsq(i) = smdlsq(i) + 
     >                        abs(qtest-qobs(i))
                              smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                        (asin(sqrt(qtest)) - 
     >                        asin(sqrt(qobs(i)))))
                           endif
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_ort
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for an orthorhombic cell.
c
      include 'puder.cmn'

      integer h,k,l
      logical extinct
      integer hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(sqrt((qobs(i)+qdel(i))/g11))
            do h = 0,hend
               rh2g11 = real(h*h)*g11
               rest = (qobs(i)+qdel(i)-rh2g11) / g22
               if (rest .gt. 0.0) then
                  kend = nint(sqrt(rest))
               else
                  kend = 0
               endif
               do k = 0,kend
                  rk2g22 = real(k*k)*g22
                  rest = (qobs(i)+qdel(i)-rh2g11-rk2g22) / g33
                  if (rest .gt. 0.0) then
                     lend = nint(sqrt(rest))
                  else
                     lend = 0
                  endif
                  do l = 0,lend
                     if (.not.extinct(h,k,l)) then		
                        qtest = rh2g11+rk2g22+real(l*l)*g33
                        absdel = abs(qtest-qobs(i))
                        if (absdel .lt. qdel(i) ) then
                           smdlsq(i) = smdlsq(i) + abs(qtest-qobs(i))
                           smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                     (asin(sqrt(qtest)) - asin(sqrt(qobs(i)))))
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_mon
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for a monoclinic cell.
c
      include 'puder.cmn'

      integer h,k,l
      logical extinct
      integer lbeg,hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hend = nint(1.42*sqrt((qobs(i)+qdel(i))/g11))
            kend = nint(sqrt((qobs(i)+qdel(i))/g22))
            lend = nint(1.42*sqrt((qobs(i)+qdel(i))/g33))
            do h = 0,hend
               do k = 0,kend
                  if (h .eq. 0) then
                     lbeg = 0
                  else
                     lbeg = -lend
                  endif
                  do l = lbeg,lend
                     if (.not.extinct(h,k,l)) then		
                        qtest = real(h*h)*g11+real(k*k)*g22+
     >                           real(l*l)*g33+real(h*l)*g13
                        absdel = abs(qtest-qobs(i))
                        if (absdel .lt. qdel(i) ) then
                           smdlsq(i) = smdlsq(i) + abs(qtest-qobs(i))
                           smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                     (asin(sqrt(qtest)) - asin(sqrt(qobs(i)))))
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine merit_tric
c
c --- This routine computes the elements of the delta-ssq and
c     delta-2theta arrays, for a triclinic cell.
c
      include 'puder.cmn'

      integer h,k,l
      logical extinct
      integer hbeg,kbeg,lbeg,hend,kend,lend

      do i = 1,nobs
         if (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) then
            smdlsq(i) = 0.0
            smdl2t(i) = 0.0
            hbeg = 0
            hend = nint(1.42*sqrt((qobs(i)+qdel(i))/g11))
            kend = nint(1.42*sqrt((qobs(i)+qdel(i))/g22))
            lend = nint(1.42*sqrt((qobs(i)+qdel(i))/g33))
c
c --------- loop over H, and compute limits of K.
c
            do h = hbeg,hend
               if (h .eq. 0) then
                  kbeg = 0
               else
                  kbeg = -kend
               endif
c
c ------------ loop over K, and compute limits of L.
c
               do k = kbeg,kend
                  if (h .eq. 0 .and. k .eq. 0) then
                     lbeg = 0
                  else
                     lbeg = -lend
                  endif
c
c --------------- loop over L.
c
                  do l = lbeg,lend
                     if (.not.extinct(h,k,l)) then		
                        qtest = real(h*h)*g11+real(k*k)*g22+
     >                          real(l*l)*g33+real(h*k)*g12+
     >                          real(h*l)*g13+real(k*l)*g23
                        absdel = abs(qtest-qobs(i))
                        if (absdel .lt. qdel(i) ) then
                           smdlsq(i) = smdlsq(i) + abs(qtest-qobs(i))
                           smdl2t(i) = smdl2t(i) + abs(2.0 *
     >                     (asin(sqrt(qtest)) - asin(sqrt(qobs(i)))))
                        endif
                     endif
                  enddo
               enddo
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine write_theo_line(lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                           ncal,h,k,l,qcalc,wave,no_more)
c
c --- this routine writes a theoretical line.
c
      integer   h,k,l
      logical   no_more
      character ans*1

      if (iwrite .eq. 0) then
         return
      endif

      twoth = q_2_twoth(qcalc,wave)
      dv    = q_2_d(qcalc)
c
c --- possibly store the 2theta value on file.
c
      if (ifil .eq. 1) then
         write(lu_theo_hkl,'(f10.5,3i5,f10.5)') twoth,h,k,l,dv
         return
      endif
c
c --- maybe the 2theta value should be written to the display.
c
      if (iwrite .eq. 1) then
         write(ludisp,'(i10,3i5,f10.4,f10.6)')
     >   ncal,h,k,l,twoth,qcalc
      endif

      if (mod(ncal,20) .eq. 0) then
         write(ludisp,'(a,$)') ' More ?  <Y[Def.]/N>'
         read(lukeys,'(a)') ans
         if (ans.eq.'q' .or. ans.eq.'Q' .or.
     >       ans.eq.'n' .or. ans.eq.'N') then
            no_more = .true.
            return
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      logical function ref_is_not_000(h,k,l)

      integer h,k,l

      if (h.eq.0 .and. k.eq.0 .and. l.eq.0) then
         ref_is_not_000 = .false.
      else
         ref_is_not_000 = .true.
      endif

      return
      end

c-----------------------------------------------------------------------

      function ntheor(iwrite,ifil,lukeys,ludisp,lu_theo_hkl,
     >                isyst,qlim,g11,g22,g33,g12,g13,g23,wave)
c
c --- This function calculates the theoretical number of lines with
c     different positions up to Q >= qlim.
c
c     if IWRITE = 0 the reflection will be written to LUDISP
c     if IFIL = 0 the reflection will be written to LU_THEO_HKL
c
      integer   h,hend,k,kend,l,lend
      logical   extinct
      logical   ref_is_not_000
      logical   nomoreflag

      nomoreflag = .false.
c
c --- depending on crystal system calculate the lines ...
c
c --- cubic system
c
      if (isyst .eq. 1) then
         ncal = 0
         hend = nint(sqrt(0.00001+qlim/g11))
         do h = 0,hend
            do k = 0,h
               do l = 0,k
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = (h*h+k*k+l*l)*g11
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
c
c --- tetragonal system
c
      elseif (isyst .eq. 3) then
         ncal = 0
         hend = nint(sqrt(0.00001+qlim/g11))
         lend = nint(sqrt(0.00001+qlim/g33))
         do h = 0,hend
            do k = 0,h
               do l = 0,lend
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = (h*h+k*k)*g11+l*l*g33
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
c
c --- hexagonal system
c
      elseif (isyst .eq. 4) then
         ncal = 0
         hend = nint(sqrt(0.00001+qlim/g11))
         kend = hend
         lend = nint(sqrt(0.00001+qlim/g33))
         do h = 0,hend
            do k = nint(-0.51*h),h
               do l = 0,lend
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = (h*h+h*k+k*k)*g11+l*l*g33
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
c
c --- orthorhombic system
c
      elseif (isyst .eq. 5) then
         ncal = 0
         hend = nint(sqrt(0.00001+qlim/g11))
         kend = nint(sqrt(0.00001+qlim/g22))
         lend = nint(sqrt(0.00001+qlim/g33))
         do h = 0,hend
            do k = 0,kend
               do l = 0,lend
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = h*h*g11+k*k*g22+l*l*g33
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
c
c --- monoclinic system
c
      elseif (isyst .eq. 6) then
         ncal = 0
         hend = nint(sqrt(0.00001+qlim/g11))
         kend = nint(sqrt(0.00001+qlim/g22))
         lend = 2*nint(sqrt(0.00001+qlim/g33)) 
         do h = 0,hend
            do k = 0,kend
               if (h .eq. 0) then
                  lbeg = 0
               else
                  lbeg = -lend
               endif
               do l = lbeg,lend
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = h*h*g11+k*k*g22+l*l*g33+h*l*g13
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
c
c --- triclinic system
c
      elseif (isyst .eq.7) then
         ncal = 0
         hend = 2*nint(sqrt(0.00001+qlim/g11))
         kend = 2*nint(sqrt(0.00001+qlim/g22))
         lend = 2*nint(sqrt(0.00001+qlim/g33))
         do h = 0,hend
            if (h .eq. 0) then
               kbeg = 0
            else
               kbeg = -kend
            endif
            do k = kbeg,kend
               if (h.eq.0 .and. k.eq.0) then
                  lbeg = 0
               else
                  lbeg = -lend
               endif
               do l = lbeg,lend
                  if (ref_is_not_000(h,k,l) .and. 
     >                .not.extinct(h,k,l) ) then
                     qcalc = get_qcalc
     >                         (h,k,l,g11,g22,g33,g12,g13,g23)
                     if (qcalc.le.qlim) then
                        ncal=ncal+1
                        call write_theo_line
     >                       (lukeys,ludisp,lu_theo_hkl,iwrite,ifil,
     >                        ncal,h,k,l,qcalc,wave,nomoreflag)
                        if (nomoreflag) then
                           return
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo

      endif
c
c --- set the function value that is to be returned to caller.
c     in this case, the number of theoretical lines.
c
      ntheor = ncal

      return
      end      

c-----------------------------------------------------------------------

      subroutine get_data (str,iweg)
c
c --- this routine reads spacing data.
c
      include 'puder.cmn'

      character str*80, curr*80

      if (str .eq. ' ') then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' No spacing data given as parameter(s) ?'
         return
      endif

      if (ispctyp .eq. 0) then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)')
     >   ' Sorry, you must specify type of spacing data first'
         write (ludisp,'(a,a)') 
     >   ' Use one of the commands:',
     >   ' SSQvalues, Qvalues, THeta, 2Theta or DValues'
         write (ludisp,'(a)') ' '
         return
      endif
c
c --- read spacing data.
c     first element of DATA or WDATA parameters is spacing measure.
c
      call move_first_element (str, curr)       
      call get_real(curr,temp,istat)
      if (istat .ne. 0) then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') 
     >   ' Error encountered when reading spacing data.'
         return
      endif
c
c --- possibly also a read of esd of spacing data. (if iweg=1)
c     OBS unit of ESD is 2theta
c
      if (iweg .eq. 1) then
         if (str .eq. ' ') then
            esdtemp = 0.1
         else
            call move_first_element (str, curr)       
            call get_real(curr,esdtemp,istat)
            if (istat .ne. 0) then
               write (ludisp,'(a)') ' '
               write (ludisp,'(a)') 
     >         ' Error encountered when reading ESD in WDATA command.'
               esdtemp = 0.1
            endif
            if (esdtemp .lt. smallest_acceptable_esd) then
               esdtemp = smallest_acceptable_esd
            endif
         endif
      else
         esdtemp = 0.1
      endif
c
c --- read free vaiable.
c
      if (str .eq. ' ') then
         fvtemp = 0.0
      else
         call move_first_element (str, curr)       
         call get_real(curr,fvtemp,istat)
         if (istat .ne. 0) then
            write (ludisp,'(a)') ' '
            write (ludisp,'(a,a)') 
     >      ' Error encountered when reading free variable in',
     >      ' WDATA command.'
            fvtemp = 0.0
         endif
      endif
c
c --- convert from used measure to Q.
c
      nobs = nobs+1

      if (ispctyp .eq. 1) then
         qobs(nobs) = ssq_2_q(temp,wave)

      elseif (ispctyp .eq. 2) then
         qobs(nobs) = temp

      elseif (ispctyp .eq. 3) then
         qobs(nobs) = th_2_q(temp,wave)

      elseif (ispctyp .eq. 4) then
         qobs(nobs) = twoth_2_q(temp,wave)

      elseif (ispctyp .eq. 5) then
         qobs(nobs) = d_2_q(temp)
      endif

      esdobs(nobs) = esdtemp
      freevar(nobs) = fvtemp
      weight(nobs) = 1.0

      ilock(nobs) = 0

      return
      end

c-----------------------------------------------------------------------

      subroutine get_hkls_data (str,iweg)
c
c --- this routine reads h,k,l and spacing data.
c     and weight if iweg = 1, no weight if iweg = 0
c
      include 'puder.cmn'

      character str*80, curr*80

      if (str .eq. ' ') then
         write (ludisp,'(a)') ' '
         if (iweg .eq. 1) then
            write (ludisp,'(a)') ' No h,k,l,spacing-data and weight ?'
         else
            write (ludisp,'(a)') ' No h,k,l and spacing-data ?'
         endif
         return
      endif

      if (ispctyp .eq. 0) then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)')
     >   ' Sorry, you must specify type of spacing data first'
         write (ludisp,'(a,a)') 
     >   ' Use one of the commands:',
     >   ' SSQvalues, QValues, THeta, 2Theta or DValues'
         write (ludisp,'(a)') ' '
         return
      endif

      call move_first_element (str, curr)       
      call get_int(curr,ihtemp,istath)

      call move_first_element (str, curr)       
      call get_int(curr,iktemp,istatk)

      call move_first_element (str, curr)       
      call get_int(curr,iltemp,istatl)

      call move_first_element (str, curr)       
      call get_real(curr,temp,istats)

      if (iweg .eq. 1) then
         call move_first_element (str, curr)       
         call get_real(curr,wtemp,istatw)
      endif

      if (iweg .eq. 0) then
         if (istath .ne. 0 .or. istatk .ne. 0 .or.
     >       istatl .ne. 0 .or. istats .ne. 0 ) then
             write (ludisp,'(a)') ' '
             write (ludisp,'(a)') 
     >      ' Error encountered when reading h,k,l and spacing data.'
            return
         endif
      else
         if (istath .ne. 0 .or. istatk .ne. 0 .or.
     >       istatl .ne. 0 .or. istats .ne. 0 .or.
     >       istatw .ne. 0 ) then
             write (ludisp,'(a)') ' '
             write (ludisp,'(a,a)') 
     >      ' Error encountered when reading h,k,l,spacing',
     >      ' and weight-data.'
            return
         endif
      endif

      if (str .eq. ' ') then
         fvtemp = 0.0
      else
         call move_first_element (str, curr)       
         call get_real(curr,fvtemp,istat)
         if (istat .ne. 0) then
            write (ludisp,'(a)') ' '
            if (iweg .eq. 0) then
               write (ludisp,'(a,a)') 
     >         ' Error encountered when reading free variable in',
     >         ' HKLDATA command.'
            else
               write (ludisp,'(a,a)') 
     >         ' Error encountered when reading free variable in',
     >         ' WHKLDATA command.'
            endif
            fvtemp = 0.0
         endif
      endif

      nobs = nobs+1

      ih(nobs) = ihtemp
      ik(nobs) = iktemp
      il(nobs) = iltemp

      if (ispctyp .eq. 1) then
         qobs(nobs) = ssq_2_q(temp,wave)
      elseif (ispctyp .eq. 2) then
         qobs(nobs) = temp
      elseif (ispctyp .eq. 3) then
         qobs(nobs) = th_2_q(temp,wave)
      elseif (ispctyp .eq. 4) then
         qobs(nobs) = twoth_2_q(temp,wave)
      elseif (ispctyp .eq. 5) then
         qobs(nobs) = d_2_q(temp)
      endif

      if (iweg .eq. 1) then
         weight(nobs) = wtemp
      else
         weight(nobs) = 1.0
      endif

      freevar(nobs) = fvtemp

      qcalc(nobs) = get_qcalc(ihtemp,iktemp,iltemp,
     >                     g11,g22,g33,g12,g13,g23)
      nokay(nobs) = 1
      ilock(nobs) = 1

      return
      end

c-----------------------------------------------------------------------
c     Conversion functions from/to q and differnt other quantities.
c-----------------------------------------------------------------------

      function ssq_2_q(x,wave)
      ssq_2_q = x*4.0/sqr(wave)
      return
      end

c-----------------------------------------------------------------------

      function th_2_q(x,wave)
      th_2_q = sqr(sin(torad(x)))*4.0/sqr(wave)
      return
      end

c-----------------------------------------------------------------------

      function twoth_2_q(x,wave)
      twoth_2_q = sqr(sin(torad(x*0.5)))*4.0/sqr(wave)
      return
      end

c-----------------------------------------------------------------------

      function d_2_q(x)
      d_2_q = sqr(1.0/x)
      return
      end

c-----------------------------------------------------------------------

      function q_2_ssq(x,wave)
      q_2_ssq = x*sqr(wave)*0.25
      return
      end

c-----------------------------------------------------------------------

      function q_2_th(x,wave)
      q_2_th = todeg(asin(sqrt(x*sqr(wave)*0.25)))
      return
      end

c-----------------------------------------------------------------------

      function q_2_twoth(x,wave)
      q_2_twoth = 2.0*todeg(asin(sqrt(x*sqr(wave)*0.25)))
      return
      end

c-----------------------------------------------------------------------

      function q_2_d(x)
      q_2_d = sqrt(1.0/x)
      return
      end

c-----------------------------------------------------------------------

      subroutine read_gij_tensor
     >           (g11,g22,g33,g12,g13,g23,isyst,lukeys,ludisp)
c
c --- this routine read the elements of the reciprocal metric tensor.
c     first set all elements to zero.
      tg11 = 0.0
      tg22 = 0.0
      tg33 = 0.0
      tg12 = 0.0
      tg13 = 0.0
      tg23 = 0.0
c
c --- then read temporary values
c
      if (isyst .eq. 0) then
         write(ludisp,'(a)') ' Specify crystal system first!'
         return
 
      elseif (isyst .eq. 1) then
         write(ludisp,'(a,$)') ' Give g11 :'
         read (lukeys,*,iostat=istat) tg11
         tg22 = tg11
         tg33 = tg11

      elseif (isyst .eq. 2) then
         write(ludisp,'(a,$)') ' Give g11 and g12 :'
         read (lukeys,*,iostat=istat) tg11,tg12
         tg22 = tg11
         tg33 = tg11
         tg13 = tg12
         tg23 = tg12

      elseif (isyst .eq. 3) then
         write(ludisp,'(a,$)') ' Give g11 and g33 :'
         read (lukeys,*,iostat=istat) tg11,tg33
         tg22 = tg11

      elseif (isyst .eq. 4) then
         write(ludisp,'(a,$)') ' Give g11 and g33 :'
         read (lukeys,*,iostat=istat) tg11,tg33
         tg22 = tg11
         tg12 = tg11

      elseif (isyst .eq. 5) then
         write(ludisp,'(a,$)') ' Give g11,g22 and g33 :'
         read (lukeys,*,iostat=istat) tg11,tg22,tg33

      elseif (isyst .eq. 6) then
         write(ludisp,'(a,$)') ' Give g11,g22,g33 and g13 :'
         read (lukeys,*,iostat=istat) tg11,tg22,tg33,tg13

      elseif (isyst .eq. 7) then
         write(ludisp,'(a,$)') ' Give g11,g22,g33,g12,g13 and g23 :'
         read (lukeys,*,iostat=istat) tg11,tg22,tg33,tg12,tg13,tg23

      endif
c
c --- set tensor elements to temporary values read above...
c
      if (istat .eq. 0) then
         g11 = tg11 
         g22 = tg22 
         g33 = tg33 
         g12 = tg12 
         g13 = tg13 
         g23 = tg23 
      endif

      return
      end

c-----------------------------------------------------------------------

      logical function systok(isyst,a,b,c,al,be,ga)
c
c --- this function checks the consistency between the cell parameters
c     and the crystal system.
c
      if (isyst .eq. 1) then
         if (abs(a/b-1.0) .gt. 0.00001 .or.
     >       abs(a/c-1.0) .gt. 0.00001 .or.
     >       abs(al-90.0) .gt. 0.0001  .or.
     >       abs(be-90.0) .gt. 0.0001  .or.
     >       abs(ga-90.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 2) then
         if (abs(a/b-1.0)   .gt. 0.00001 .or.
     >       abs(a/c-1.0)   .gt. 0.0001  .or.
     >       abs(al/be-1.0) .gt. 0.0001  .or.
     >       abs(al/ga-1.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 3) then
         if (abs(a/b-1.0) .gt. 0.00001 .or.
     >       abs(al-90.0) .gt. 0.0001  .or.
     >       abs(be-90.0) .gt. 0.0001  .or.
     >       abs(ga-90.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 4) then
         if (abs(a/b-1.0) .gt. 0.00001 .or.
     >       abs(al-90.0) .gt. 0.0001  .or.
     >       abs(be-90.0) .gt. 0.0001  .or.
     >       abs(ga-120.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 5) then
         if (abs(al-90.0) .gt. 0.0001  .or.
     >       abs(be-90.0) .gt. 0.0001  .or.
     >       abs(ga-90.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 6) then
         if (abs(al-90.0) .gt. 0.0001  .or.
     >       abs(ga-90.0) .gt. 0.0001  ) then
            systok = .false.
         else
            systok = .true.
         endif

      elseif (isyst .eq. 7) then
         systok = .true.

      else
         systok = .false.

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine prtsome(ikey)
c
c --- this routine prints some information.
c
      include 'puder.cmn'

      character ch*1

      if (ikey .eq. 0) then
         write (ludisp,'(a)') ' What do you want to print ?'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' 1) Real cell parameters.'
         write (ludisp,'(a)') ' 2) Reciprokal cell parameters.'
         write (ludisp,'(a)') ' 3) Reciprokal metric tensor.'
         write (ludisp,'(a)') ' 4) The observed lines.'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a,$)') ' Enter your choice here:'
         read  (lukeys,'(a)') ch
         if (ch.eq.'1' .or. ch.eq.'2' .or. ch.eq.'3' .or.
     >       ch.eq.'4') then
            write (ludisp,'(a)') ' '
         endif
      endif

      if (ch .eq. '1' .or. ikey .eq. 1) then
         real_volume = vol (a,b,c,torad(al),torad(be),torad(ga))
         if (siga .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' a ......:',a,' +/- ',siga,'    p/sig(p): ',a/siga
         else
            write (ludisp,'(a,f10.5)')
     >      ' a ......:',a
         endif
         if (sigb .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' b ......:',b,' +/- ',sigb,'    p/sig(p): ',b/sigb
         else
            write (ludisp,'(a,f10.5)')
     >      ' b ......:',b
         endif
         if (sigc .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' c ......:',c,' +/- ',sigc,'    p/sig(p): ',c/sigc
         else
            write (ludisp,'(a,f10.5)')
     >      ' c ......:',c
         endif
         if (sigal .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' alfa ...:',al,' +/- ',sigal,'    p/sig(p): ',al/sigal
         else
            write (ludisp,'(a,f10.5)')
     >      ' alfa ...:',al
         endif
         if (sigbe .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' beta ...:',be,' +/- ',sigbe,'    p/sig(p): ',be/sigbe
         else
            write (ludisp,'(a,f10.5)')
     >      ' beta ...:',be
         endif
         if (sigga .gt. 1.0e-10) then
            write (ludisp,'(a,f10.5,a,f10.5,a,f10.1)')
     >      ' gamma ..:',ga,' +/- ',sigga,'    p/sig(p): ',ga/sigga
         else
            write (ludisp,'(a,f10.5)')
     >      ' gamma ..:',ga
         endif
         write (ludisp,'(a,f10.3)') ' Volume .:',real_volume
         if (ilogfl .eq. 1) then
            if (siga .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' a ......:',a,' +/- ',siga,'    p/sig(p): ',a/siga
            else
               write (lulog,'(a,f10.5)')
     >         ' a ......:',a
            endif
            if (sigb .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' b ......:',b,' +/- ',sigb,'    p/sig(p): ',b/sigb
            else
               write (lulog,'(a,f10.5)')
     >         ' b ......:',b
            endif
            if (sigc .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' c ......:',c,' +/- ',sigc,'    p/sig(p): ',c/sigc
            else
               write (lulog,'(a,f10.5)')
     >         ' c ......:',c
            endif
            if (sigal .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' alfa ...:',al,' +/- ',sigal,'    p/sig(p): ',al/sigal
            else
               write (lulog,'(a,f10.5)')
     >         ' alfa ...:',al
            endif
            if (sigbe .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' beta ...:',be,' +/- ',sigbe,'    p/sig(p): ',be/sigbe
            else
               write (lulog,'(a,f10.5)')
     >         ' beta ...:',be
            endif
            if (sigga .gt. 1.0e-10) then
               write (lulog,'(a,f10.5,a,f10.5,a,f10.1)')
     >         ' gamma ..:',ga,' +/- ',sigga,'    p/sig(p): ',ga/sigga
            else
               write (lulog,'(a,f10.5)')
     >         ' gamma ..:',ga
            endif
            write (lulog,'(a,f10.3)') ' Volume .:',real_volume
         endif

      elseif (ch .eq. '2' .or. ikey .eq. 2) then
         reciprocal_volume =
     >   vol (ast,bst,cst,torad(alst),torad(best),torad(gast))
         write (ludisp,'(a,f10.7,a,f10.7)')
     >   ' a* .....:',ast,' +/- ',sigast
         write (ludisp,'(a,f10.7,a,f10.7)')
     >   ' b* .....:',bst,' +/- ',sigbst
         write (ludisp,'(a,f10.7,a,f10.7)')
     >   ' c* .....:',cst,' +/- ',sigcst
         write (ludisp,'(a,f10.5,a,f10.5)')
     >   ' alfa* ..:',alst,' +/- ',sigalst
         write (ludisp,'(a,f10.5,a,f10.5)')
     >   ' beta* ..:',best,' +/- ',sigbest
         write (ludisp,'(a,f10.5,a,f10.5)')
     >   ' gamma* .:',gast,' +/- ',siggast
         write (ludisp,'(a,f10.8)') ' Volume .:',reciprocal_volume
         if (ilogfl .eq. 1) then
            write (lulog,'(a,f10.7,a,f10.7)')
     >      ' a* .....:',ast,' +/- ',sigast
            write (lulog,'(a,f10.7,a,f10.7)')
     >      ' b* .....:',bst,' +/- ',sigbst
            write (lulog,'(a,f10.7,a,f10.7)')
     >      ' c* .....:',cst,' +/- ',sigcst
            write (lulog,'(a,f10.5,a,f10.5)')
     >      ' alfa* ..:',alst,' +/- ',sigalst
            write (lulog,'(a,f10.5,a,f10.5)')
     >      ' beta* ..:',best,' +/- ',sigbest
            write (lulog,'(a,f10.5,a,f10.5)')
     >      ' gamma* .:',gast,' +/- ',siggast
            write (lulog,'(a,f10.8)') ' Volume .:',reciprocal_volume
         endif

      elseif (ch .eq. '3' .or. ikey .eq. 3) then
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g11 ....:',g11,' +/- ',sigg11
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g22 ....:',g22,' +/- ',sigg22
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g33 ....:',g33,' +/- ',sigg33
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g12 ....:',g12,' +/- ',sigg12
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g13 ....:',g13,' +/- ',sigg13
         write (ludisp,'(a,f10.7,a,f10.9)')
     >   ' g23 ....:',g23,' +/- ',sigg23
         if (ilogfl .eq. 1) then
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g11 ....:',g11,' +/- ',sigg11
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g22 ....:',g22,' +/- ',sigg22
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g33 ....:',g33,' +/- ',sigg33
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g12 ....:',g12,' +/- ',sigg12
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g13 ....:',g13,' +/- ',sigg13
            write (ludisp,'(a,f10.7,a,f10.9)')
     >      ' g23 ....:',g23,' +/- ',sigg23
         endif

      elseif (ch .eq. '4' .or. ikey .eq. 4) then
         call sqwrit (wr_default)

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine setwave(wave,lukeys,ludisp,str)
c
c --- this routine sets the wavelength parameter.
c
      character str*80

      if (str .eq. ' ') then
************************************************************************
         if (wave .ge. 100.0) then
          write (ludisp,'(a,f8.4,a,$)') ' Give wavelength [',wave,'] :'
         elseif (wave .ge. 10.0) then
          write (ludisp,'(a,f8.5,a,$)') ' Give wavelength [',wave,'] :'
         elseif (wave .ge. 1.0) then
          write (ludisp,'(a,f8.6,a,$)') ' Give wavelength [',wave,'] :'
         elseif (wave .ge. 0.1) then
          write (ludisp,'(a,f8.7,a,$)') ' Give wavelength [',wave,'] :'
         endif
         read (lukeys,'(a)') str
         if (str.eq.' ') then
            write(ludisp,'(a)') ' No change done ...'
            return
         endif
      endif
      if (str .eq. ' ') then
         return
      endif
      call strip(str)
      call upline(str)
      if (str .eq. '?' .or.
     >    str .eq. 'HELP' .or.
     >    str .eq. 'HEL' .or.
     >    str .eq. 'HE' .or.
     >    str .eq. 'H' ) then
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') 
     >   ' Possible arguments to the command WAVE are:'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '   CrKa   =  2.28 A'
         write (ludisp,'(a)') '   CrKa1  =  2.28 A'
         write (ludisp,'(a)') '   CrKa2  =  2.28 A'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '   CoKa   =   A'
         write (ludisp,'(a)') '   CoKa1  =   A'
         write (ludisp,'(a)') '   CoKa2  =   A'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '   CuKa   =  1.54 A'
         write (ludisp,'(a)') '   CuKa1  =  1.54060 A'
         write (ludisp,'(a)') '   CuKa2  =  1.54 A'
         write (ludisp,'(a)') '   CuKb   =  1.45 A'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '   MoKa   =  0.71073 A'
         write (ludisp,'(a)') '   MoKa1  =  0.71073 A'
         write (ludisp,'(a)') '   MoKa2  =  0.71073 A'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '   AgKa   =  0.56 A'
         write (ludisp,'(a)') '   AgKa1  =  0.56 A'
         write (ludisp,'(a)') '   AgKa2  =  0.56 A'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') '  -or a numeric value of the wavelength!'
         return

      elseif (str .eq. 'CUKA1') then
         wave = 1.54060
         cl2q = wave*wave*0.25

      elseif (str .eq. 'MOKA1') then
         wave = 0.71073
         cl2q = wave*wave*0.25

      else
         call get_real(str,temp,istat)
         if (istat .eq. 0) then
            wave = temp
            cl2q = wave*wave*0.25
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      logical function okqobs(ludisp,qobs,nobs,wave)

      dimension qobs(1)

      qmin = qobs(1)
      qmax = qobs(1)
      do i = 1,nobs
         qmin = min(qmin,qobs(i))
         qmax = max(qmax,qobs(i))
      enddo

      if (qmin .lt. 0.0 .or. qmax .gt. 4.0/sqr(wave)) then
         okqobs = .false.
         write (ludisp,'(a)') 
     >   ' Strange, the Q-values seem to be unphysical...'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a,f10.5)') ' Min values of qobs is:',qmin
         write (ludisp,'(a,f10.5)') ' Max values of qobs is:',qmax
         write (ludisp,'(a)') ' '
         write (ludisp,'(a)') ' This corresponds to 2theta > 180 deg.'
         write (ludisp,'(a)') ' '
         write (ludisp,'(a,a)') 
      else
         okqobs = .true.
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine wrdef_elements (ludisp)

      write(ludisp,'(a)') ' '
      write(ludisp,'(a,a)')
     >' You can issue one or several of the following',
     >' qualifiers.'
      write(ludisp,'(a)') ' '
      write(ludisp,'(a)')
     >' The letters inside "()" are optional but must be written'
      write(ludisp,'(a)')
     >' from left to right if they are present.'
      write(ludisp,'(a)') ' '
      write(ludisp,'(a)')
     >' N .............: Ordinal number of line.'
      write(ludisp,'(a)')
     >' H .............: H-index.'
      write(ludisp,'(a)')
     >' K .............: K-index.'
      write(ludisp,'(a)')
     >' L .............: L-index.'
      write(ludisp,'(a)')
     >' QO(BS) ........: Observed Q value.'
      write(ludisp,'(a)')
     >' QC(ALC) .......: Calculated Q value.'
      write(ludisp,'(a)')
     >' QD(EL) ........: Delta Q (obs-calc).'
      write(ludisp,'(a)')
     >' 2(THETA)O(BS) .: Observed 2-theta value.'
      write(ludisp,'(a)')
     >' 2(THETA)C(ALC) : Calculated 2-theta value.'
      write(ludisp,'(a)')
     >' 2(THETA)D(EL) .: Delta 2-theta (obs-calc).'
      write(ludisp,'(a)')
     >' DO(BS) ........: Observed d-value.'
      write(ludisp,'(a)')
     >' DC(ALC) .......: Calculated d-value.'
      write(ludisp,'(a)')
     >' DD(EL) ........: Delta d-value (obs-calc).'
      write(ludisp,'(a)')
     >' ESD(OBS) ......: Esd(observed spacing data).'
      write(ludisp,'(a)')
     >' W(EIGHT) ......: The weight used in the last refinement.'
      write(ludisp,'(a)')
     >' F(VAR)/(REE) ..: Free variable.'
      write(ludisp,'(a)')
     >' NO(KAY) .......: Number of lines that fit the observed one.'
      write(ludisp,'(a)') ' '

      return
      end

c-----------------------------------------------------------------------

      subroutine sqwrit (cpar)
c
c --- this routine writes the indexed pattern.
c
      include 'puder.cmn'

      character str*80, curr*80, cpar*80,
     >          wri_line*80, tit_line*80

      logical   wrn,wrh,wrk,wrl,wrqo,wrqc,wrqd,
     >          wr2tho,wr2thc,wr2thd,wrdvo,wrdvc,wrdvd,
     >          wrweig,wresd,wrfree,wrnokay

      integer   iwr(40),nwr

      logical   okqobs

      if (cpar .eq. '?') then
         call wrdef_elements (ludisp)
         return
      endif
c
c --- first check that the qobs array do contain valid 
c     sine-square theta values.
c
      if (.not.okqobs(ludisp,qobs,nobs,wave)) then
         return
      endif
c
c --- decide what to write ...
c
      wrn = .false.
      wrh = .false.
      wrk = .false.
      wrl = .false.
      wrqo = .false.
      wrqc = .false.
      wrqd = .false.
      wr2tho = .false.
      wr2thc = .false.
      wr2thd = .false.
      wrdvo = .false.
      wrdvc = .false.
      wrdvd = .false.
      wresd = .false.
      wrweig = .false.
      wrfree = .false.
      wrnokay = .false.

      tit_line = ' '
      itptr = 1
c
c --- copy the argument to the work string "str", in order to avoid any
c     side-effects.
c
      str = cpar
      nwr = 0

      call upline(str)
      call strip(str)
c
c --- decide what is to be written and set the title string.
c
 999  continue
      if (str .ne. ' ') then
         call move_first_element (str,curr)

         if (curr .eq. 'N') then
            wrn = .true.
            tit_line(itptr:itptr+3) = '   N'
            itptr = itptr+4
            nwr = nwr+1
            iwr(nwr) = 1

         elseif (curr .eq. 'H') then
            wrh = .true.
            tit_line(itptr:itptr+3) = '   H'
            itptr = itptr+4
            nwr = nwr+1
            iwr(nwr) = 2

         elseif (curr .eq. 'K') then
            wrk = .true.
            tit_line(itptr:itptr+3) = '   K'
            itptr = itptr+4
            nwr = nwr+1
            iwr(nwr) = 3

         elseif (curr .eq. 'L') then
            wrl = .true.
            tit_line(itptr:itptr+3) = '   L'
            itptr = itptr+4
            nwr = nwr+1
            iwr(nwr) = 4

         elseif (curr .eq. 'QOBS' .or.
     >           curr .eq. 'QOB' .or.
     >           curr .eq. 'QO' ) then
            wrqo = .true.
            tit_line(itptr:itptr+9) = '     Q-obs'
            itptr = itptr+10
            nwr = nwr+1
            iwr(nwr) = 5

         elseif (curr .eq. 'QCALC' .or.
     >           curr .eq. 'QCAL' .or.
     >           curr .eq. 'QCA' .or.
     >           curr .eq. 'QC' ) then
            wrqc = .true.
            tit_line(itptr:itptr+9) = '    Q-calc'
            itptr = itptr+10
            nwr = nwr+1
            iwr(nwr) = 6

         elseif (curr .eq. 'QDEL' .or. 
     >           curr .eq. 'QDE' .or. 
     >           curr .eq. 'QD' ) then
            wrqd = .true.
            tit_line(itptr:itptr+9) = '     Q-del'
            itptr = itptr+10
            nwr = nwr+1
            iwr(nwr) = 7

         elseif (curr .eq. '2THETAOBS' .or.
     >           curr .eq. '2THETAOB' .or.
     >           curr .eq. '2THETAO' .or.
     >           curr .eq. '2THETO' .or.
     >           curr .eq. '2THEO' .or.
     >           curr .eq. '2THO' .or.
     >           curr .eq. '2TO' .or.
     >           curr .eq. '2O' ) then
            wr2tho = .true.
            tit_line(itptr:itptr+8) = '  2Th-obs'
            itptr = itptr+9
            nwr = nwr+1
            iwr(nwr) = 8

         elseif (curr .eq. '2THETACALC' .or.
     >           curr .eq. '2THETACAL' .or.
     >           curr .eq. '2THETACA' .or.
     >           curr .eq. '2THETAC' .or.
     >           curr .eq. '2THETC' .or.
     >           curr .eq. '2THEC' .or.
     >           curr .eq. '2THC' .or.
     >           curr .eq. '2TC' .or.
     >           curr .eq. '2C' ) then
            wr2thc = .true.
            tit_line(itptr:itptr+8) = ' 2Th-calc'
            itptr = itptr+9
            nwr = nwr+1
            iwr(nwr) = 9

         elseif (curr .eq. '2THETADEL' .or.
     >           curr .eq. '2THETADE' .or.
     >           curr .eq. '2THETAD' .or.
     >           curr .eq. '2THETAD' .or.
     >           curr .eq. '2THETD' .or.
     >           curr .eq. '2THED' .or.
     >           curr .eq. '2THD' .or.
     >           curr .eq. '2TD' .or.
     >           curr .eq. '2D' ) then
            wr2thd = .true.
            tit_line(itptr:itptr+8) = '  2Th-del'
            itptr = itptr+9
            nwr = nwr+1
            iwr(nwr) = 10

         elseif (curr .eq. 'DOBS' .or.
     >           curr .eq. 'DOB' .or.
     >           curr .eq. 'DO' ) then
            wrdvo = .true.
            tit_line(itptr:itptr+7) = '   d-obs'
            itptr = itptr+8
            nwr = nwr+1
            iwr(nwr) = 11

         elseif (curr .eq. 'DCALC' .or.
     >           curr .eq. 'DCAL' .or.
     >           curr .eq. 'DCA' .or.
     >           curr .eq. 'DC' ) then
            wrdvc = .true.
            tit_line(itptr:itptr+7) = '  d-calc'
            itptr = itptr+8
            nwr = nwr+1
            iwr(nwr) = 12

         elseif (curr .eq. 'DDEL' .or.
     >           curr .eq. 'DDE' .or.
     >           curr .eq. 'DD' ) then
            wrdvd = .true.
            tit_line(itptr:itptr+7) = '   d-del'
            itptr = itptr+8
            nwr = nwr+1
            iwr(nwr) = 13

         elseif (curr .eq. 'ESDOBS' .or.
     >           curr .eq. 'ESDOB' .or.
     >           curr .eq. 'ESDO' .or.
     >           curr .eq. 'ESD' ) then
            wresd = .true.
            tit_line(itptr:itptr+7) = ' Esd-obs'
            itptr = itptr+8
            nwr = nwr+1
            iwr(nwr) = 17

         elseif (curr .eq. 'WEIGHT' .or.
     >           curr .eq. 'WEIGH' .or.
     >           curr .eq. 'WEIG' .or.
     >           curr .eq. 'WEI' .or.
     >           curr .eq. 'WE' .or.
     >           curr .eq. 'W' ) then
            wrweig = .true.
            tit_line(itptr:itptr+7) = '  Weight'
            itptr = itptr+8
            nwr = nwr+1
            iwr(nwr) = 14

         elseif (curr .eq. 'FVAR' .or.
     >           curr .eq. 'FVA' .or.
     >           curr .eq. 'FV' .or.
     >           curr .eq. 'F' .or.
     >           curr .eq. 'FREE' .or.
     >           curr .eq. 'FRE' .or.
     >           curr .eq. 'FR' ) then
            wrfree = .true.
            tit_line(itptr:itptr+9) = '      FVAR'
            itptr = itptr+10
            nwr = nwr+1
            iwr(nwr) = 15

         elseif (curr .eq. 'NOKAY' .or.
     >           curr .eq. 'NOKA' .or.
     >           curr .eq. 'NOK' .or.
     >           curr .eq. 'NO' ) then
            wrnokay = .true.
            tit_line(itptr:itptr+2) = '  #'
            itptr = itptr+3
            nwr = nwr+1
            iwr(nwr) = 16

         endif
         goto 999
      endif
c
c --- check that not too much have been written...
c
      if (itptr .gt. 77) then
         write (ludisp,'(a)') ' ERROR, too many arguments to write...'
         return
      endif
c
c --- write title line.
c
      write(ludisp,'(a)') tit_line(1:lastch(tit_line))
      if (ilogfl .eq. 1) then
         write(lulog,'(a)') tit_line(1:lastch(tit_line))
      endif

      nsingle = 0
      nunindexed = 0
c
c --- now write each line.
c
      do i = 1,nobs
c
c ------ initiate the write string.
c
         wri_line = ' '
         iwptr = 1
c
c ------ loop over all possible items.
c
         do k = 1,nwr
c
c --------- N = ordinal number of line in data-set.
c
            if (wrn .and. iwr(k) .eq. 1) then
               write (wri_line(iwptr:iwptr+3),'(i4)') i
               iwptr = iwptr+4
            endif
c
c --------- H-index
c
            if (wrh .and. iwr(k) .eq. 2) then
               if (nokay(i) .ge. 1) then
                  write (wri_line(iwptr:iwptr+3),'(i4)') ih(i)
               else
                  write (wri_line(iwptr:iwptr+3),'(a4)') '    '
               endif
               iwptr = iwptr+4
            endif
c
c --------- K-index
c
            if (wrk .and. iwr(k) .eq. 3) then
               if (nokay(i) .ge. 1) then
                  write (wri_line(iwptr:iwptr+3),'(i4)') ik(i)
               else
                  write (wri_line(iwptr:iwptr+3),'(a4)') '    '
               endif
               iwptr = iwptr+4
            endif
c
c --------- L-index.
c
            if (wrl .and. iwr(k) .eq. 4) then
               if (nokay(i) .ge. 1) then
                  write (wri_line(iwptr:iwptr+3),'(i4)') il(i)
               else
                  write (wri_line(iwptr:iwptr+3),'(a4)') '    '
               endif
               iwptr = iwptr+4
            endif
c
c --------- observed Q
c
            if (wrqo .and. iwr(k) .eq. 5) then
               value = qobs(i)
               write (wri_line(iwptr:iwptr+9),'(f10.6)') value
               iwptr = iwptr+10
            endif
c
c --------- calculated Q theta
c
            if (wrqc .and. iwr(k) .eq. 6) then
               if (nokay(i) .ge. 1) then
                  value = qcalc(i)
                  write (wri_line(iwptr:iwptr+9),'(f10.6)') value
               else
                  write (wri_line(iwptr:iwptr+9),'(a)') '          '
               endif
               iwptr = iwptr+10
            endif
c
c --------- delta Q ie qobs-qcalc
c
            if (wrqd .and. iwr(k) .eq. 7) then
               if (nokay(i) .ge. 1) then
                  value = qobs(i)-qcalc(i)
                  write (wri_line(iwptr:iwptr+9),'(f10.6)') value
               else
                  write (wri_line(iwptr:iwptr+9),'(a)') '          '
               endif
               iwptr = iwptr+10
            endif
c
c --------- 2theta obs.
c
            if (wr2tho .and. iwr(k) .eq. 8) then
               if (qobs(i) .ge. 0.0) then
                  value = q_2_twoth(qobs(i),wave)
                  write (wri_line(iwptr:iwptr+8),'(f9.4)') value
               else
                  write (wri_line(iwptr:iwptr+8),'(a)') ' ********'
               endif
               iwptr = iwptr+9
            endif
c
c --------- 2 theta calc.
c
            if (wr2thc .and. iwr(k) .eq. 9) then
               if (nokay(i) .ge. 1 .and. 
     >             qcalc(i) .ge. 0.0) then
                  value = q_2_twoth(qcalc(i),wave)
                  write (wri_line(iwptr:iwptr+8),'(f9.4)') value
               else
                  write (wri_line(iwptr:iwptr+8),'(a)') '        '
               endif
               iwptr = iwptr+9
            endif
c
c --------- delta 2theta.
c
            if (wr2thd .and. iwr(k) .eq. 10) then
               if (nokay(i) .ge. 1 .and.
     >             qobs(i) .ge. 0.0 .and.
     >             qcalc(i) .ge. 0.0) then
                  value = q_2_twoth(qobs(i),wave)-
     >                    q_2_twoth(qcalc(i),wave)
                  write (wri_line(iwptr:iwptr+8),'(f9.4)') value
               else
                  write (wri_line(iwptr:iwptr+8),'(a)') '        '
               endif
               iwptr = iwptr+9
            endif
c
c --------- Observed d-value.
c
            if (wrdvo .and. iwr(k) .eq. 11) then
               if (qobs(i) .gt. 0.0) then
                  value = q_2_d(qobs(i))
                  write (wri_line(iwptr:iwptr+7),'(f8.5)') value
               else
                  write (wri_line(iwptr:iwptr+7),'(a)') ' *******'
               endif
               iwptr = iwptr+8
            endif
c
c --------- Calculated d-value.
c
            if (wrdvc .and. iwr(k) .eq. 12) then
               if (nokay(i) .ge. 1 .and.     
     >             qcalc(i) .gt. 0.0) then
                  value = q_2_d(qcalc(i))
                  write (wri_line(iwptr:iwptr+7),'(f8.5)') value
               else
                  write (wri_line(iwptr:iwptr+7),'(a)') '        '
               endif
               iwptr = iwptr+8
            endif
c
c --------- Delta d-value, ie obs-calc.
c
            if (wrdvd .and. iwr(k) .eq. 13) then
               if (nokay(i) .ge. 1 .and.     
     >             qobs(i)  .gt. 0.0 .and.
     >             qcalc(i) .gt. 0.0) then
                  value = q_2_d(qobs(i))-q_2_d(qcalc(i))
                  write (wri_line(iwptr:iwptr+7),'(f8.5)') value
               else
                  write (wri_line(iwptr:iwptr+7),'(a)') '        '
               endif
               iwptr = iwptr+8
            endif
c
c --------- Weigth of this very line.
c
            if (wrweig .and. iwr(k) .eq. 14) then
               if (nokay(i) .ge. 1) then
                  write (wri_line(iwptr:iwptr+7),'(f8.4)') weight(i)
               else
                  write (wri_line(iwptr:iwptr+7),'(a)') '        '
               endif
               iwptr = iwptr+8
            endif
c
c --------- Esdobs of this very line.
c
            if (wresd .and. iwr(k) .eq. 17) then
               if (qobs(i) .gt. 0.0) then
                  write (wri_line(iwptr:iwptr+7),'(f8.4)') esdobs(i)
               else
                  write (wri_line(iwptr:iwptr+7),'(a)') '        '
               endif
               iwptr = iwptr+8
            endif
c
c --------- Free variable.
c
            if (wrfree .and. iwr(k) .eq. 15) then
               write (wri_line(iwptr:iwptr+9),'(f10.4)') freevar(i)
               iwptr = iwptr+10
            endif
c
c --------- Nokay, ie the number of lines that fit this observed line.
c
            if (wrnokay .and. iwr(k) .eq. 16) then
               write (wri_line(iwptr:iwptr+2),'(i3)') nokay(i)
               iwptr = iwptr+3
            endif

         enddo
c
c ------ locked indexes for this line or not ?
c
         if (ilock(i) .eq. 1) then
            wri_line(iwptr:iwptr+1) = ' L'
         endif
c
c ------ at last write the line.
c
         write (ludisp,'(a)') wri_line(1:lastch(wri_line))
         if (ilogfl .eq. 1) then
            write (lulog,'(a)') wri_line(1:lastch(wri_line))
         endif
c
c ------ count the number of single indexed lines.
c
         if (nokay(i) .eq. 1) then
            nsingle = nsingle+1
         endif
c
c ------ count the number of unindexed lines.
c
         if (nokay(i) .eq. 0) then
            nunindexed = nunindexed+1
         endif

      enddo

      ncalc = 0
      do i = 1,nobs
         ncalc = ncalc+nokay(i)
      enddo

      write (ludisp,'(a)') ' '
      write (ludisp,'(a,i5)')
     >' Number of observed lines .........:',nobs
      write (ludisp,'(a,i5)')
     >' Number of calculated lines .......:',ncalc
      write (ludisp,'(a,i5)')
     >' Number of single indexed lines ...:',nsingle
      write (ludisp,'(a,i5)')
     >' Number of unindexed lines ........:',nunindexed

      if (ilogfl .eq. 1) then
         write (lulog,'(a)') ' '
         write (lulog,'(a,i5)')
     >   ' Number of observed lines .........:',nobs
         write (lulog,'(a,i5)')
     >   ' Number of calculated lines .......:',ncalc
         write (lulog,'(a,i5)')
     >   ' Number of single indexed lines ...:',nsingle
         write (lulog,'(a,i5)')
     >   ' Number of unindexed lines ........:',nunindexed
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine indlin (cmdpar)
c
c --- This routine indexes the diffraction lines and thereby generates 
c     a cell.
c
      include 'puder.cmn'

      character cmdpar*80, str*80
      integer   first_line, last_line
c
c --- set the first and last line number to be indexed.
c
      if (cmdpar .eq. ' ') then
         first_line = 1
         last_line = nobs
      else

c ------ decode first argument

         call move_first_element (cmdpar, str)       
         call get_int(str,first_line,istat)
         if (istat .eq. 0) then
            if (cmdpar .eq. ' ') then
               last_line = first_line
            else

c ------------ decode second argument

               call move_first_element (cmdpar, str)       
               call get_int(str,last_line,istat)
               if (istat .ne. 0) then
                  write(ludisp,'(a)') 
     >            ' Error interpreting last line-number as an integer.'
                  return
               endif  
            endif  
         else
            write(ludisp,'(a)') 
     >      ' Error interpreting first line-number as an integer.'
            return
         endif
      endif
c
c --- check that the lines are whithin observed range.
c
      if (first_line .lt. 1) then
         first_line = 1
      endif
      if (last_line .gt. nobs) then
         last_line = nobs
      endif
c
c --- set the error limits for the indexing to be accepted.
c
      call set_error_limits
c
c --- Choose crystal system and index the lines...
c
      if (isyst .eq. 1) then
         call indcub (first_line, last_line)
      elseif (isyst .eq. 2) then
         call indtrig (first_line, last_line)
      elseif (isyst .eq. 3) then
         call indtet (first_line, last_line)
      elseif (isyst .eq. 4) then
         call indhex (first_line, last_line)
      elseif (isyst .eq. 5) then
         call indort (first_line, last_line)
      elseif (isyst .eq. 6) then
         call indmon (first_line, last_line)
      elseif (isyst .eq. 7) then
         call indtric (first_line, last_line)
      else
         write (ludisp,'(a)')
     >   ' Unknown crystal system, no indexing done ...'
      endif
c
c --- check that the indexed pattern contains only unique indexes,
c     ie check to see if two lines have the same indexes, in that case
c     notify the user.
c
      call chklin (ludisp,ih,ik,il,qobs,qdel,nokay,nobs,
     >             lulog,ilogfl)

      return
      end

c-----------------------------------------------------------------------

      subroutine set_error_limits
c
c --- Set max allowed difference between observed and calculated
c     Q values...
c
      include 'puder.cmn'

      do i = 1,nobs
         temp = q_2_twoth(qobs(i),wave)
         temp_low = temp-delta_twotheta
         temp_high = temp+delta_twotheta
         if (temp_low .lt. 0.0) then
            temp_low = 0.0
         endif
         if (temp_high .gt. 180.0) then
            temp_high = 180.0
         endif
         qlow_temp = twoth_2_q(temp_low,wave)
         qhigh_temp = twoth_2_q(temp_high,wave)
         qdel(i) = 0.5*(qhigh_temp-qlow_temp)
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine chklin (ludisp,ih,ik,il,qobs,qdel,nokay,nobs,
     >                   lulog,ilogfl)

      dimension ih(1),ik(1),il(1),qobs(1),qdel(1),nokay(1)

      iwarn = 0
      do i = 1,nobs
         if (nokay(i) .gt. 0) then
            if (iwarn .gt. 10) then
               write (ludisp,'(a)') ' Etc.'
               if (ilogfl .eq. 1) then
                  write (lulog,'(a)') ' Etc.'
               endif
               return
            endif
            qlo = qobs(i)-2.0*qdel(i)
            qhi = qobs(i)+2.0*qdel(i)
            call getpos (ibeg,qobs,i,qlo,nobs)
            call getpos (iend,qobs,i,qhi,nobs)
            do j = ibeg,iend
               if (i.ne.j .and.
     >             ih(i).eq.ih(j) .and.
     >             ik(i).eq.ik(j) .and. 
     >             il(i).eq.il(j)) then
                  iwarn = iwarn+1
                  write (ludisp,'(a,i4,a)')
     >            ' WARNING, indexes of line:',i,' are not unique.'
                  if (ilogfl .eq. 1) then
                     write (ludisp,'(a,i4,a)')
     >               ' WARNING, indexes of line:',i,' are not unique.'
                  endif
               endif
            enddo
         endif
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine getpos (ix,qobs,i1,qval,ixmax)

      dimension qobs(1)

      if (qval .lt. qobs(i1)) then
         do i = i1-1,1,-1
            if (qobs(i) .lt. qval .or. ix .eq. 1) then
               ix = i
               return
            endif
         enddo
         
      else
         do i = i1+1,ixmax
            if (qobs(i) .gt. qval .or. ix .eq. ixmax) then
               ix = i
               return
            endif
         enddo
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_line(ludisp,lulog,ilogfl,i,h,k,l,qo,qc,wave)

      integer h,k,l

      qd = qo - qc

      twoth_obs  = (q_2_twoth(qo,wave))
      twoth_calc = (q_2_twoth(qc,wave))
      twoth_diff = twoth_obs - twoth_calc

      write (ludisp,'(i5,a,3i4,3f10.6,3f10.4)')
     >i,':',h,k,l,qo,qc,qd,twoth_obs,twoth_calc,twoth_diff

      if (ilogfl .eq. 1) then
         write (lulog,'(i5,a,3i4,3f10.6,3f10.4)') 
     >   i,':',h,k,l,qo,qc,qd,twoth_obs,twoth_calc,twoth_diff
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_not_ind_line (ludisp,lulog,ilogfl,i,qobs,wave)

      twoth_obs  = (q_2_twoth(qobs,wave))
      twoth_obs = 2.0*todeg(asin(sqrt(qobs)))

      write (ludisp,'(i5,a,f10.6,a,f10.4)')
     >i,':            ',qobs,'          ',twoth_obs

      if (ilogfl .eq. 1) then
         write (lulog,'(i5,a,f10.6,a,f10.4)')
     >   i,':            ',qobs,'          ',twoth_obs
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine write_line_separator (ludisp,lulog,ilogfl)

      write(ludisp,'(a,a)')
     >' -----------------------------------------------',
     >'------------------------------'

      if (ilogfl .eq. 1) then
         write(lulog,'(a,a)')
     >   ' -----------------------------------------------',
     >   '------------------------------'
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine indcub (first_line, last_line)
c
c --- this routine indexes a cubic cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest,h2k2l2,old_h2k2l2
      logical extinct
      integer hend,kend,lend
      integer first_line,last_line

      do i = first_line, last_line
         hend = nint(sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         hbest = 0
         kbest = 0
         lbest = 0
         bestdel = 1.0
         nokay(i) = 0
         old_h2k2l2 = 0
	 do h = 0,hend
            kend = h
            if (ikset .eq. 1) then
               kend = min(kend,ikmax)
            endif
            do k = 0,kend
               lend = k
               if (ilset .eq. 1) then
                  lend = min(lend,ilmax)
               endif
               do l = 0,lend
                  if (.not.extinct(h,k,l)) then		
                     h2k2l2 = h*h+k*k+l*l
                     qtest = real(h2k2l2)*g11
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (h2k2l2 .ne. old_h2k2l2) then
                           old_h2k2l2 = h2k2l2
                           if (absdel .lt. bestdel) then
                              bestdel = absdel
                              hbest = h
                              kbest = k
                              lbest = l
                           endif
                           nokay(i) = nokay(i)+1
                           if (iverb .eq. 1) then
                              call write_line(ludisp,lulog,ilogfl,
     >                        i,h,k,l,qobs(i),qtest,wave)
                           endif
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo

         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = (ih(i)*ih(i)+ik(i)*ik(i)+il(i)*il(i))*g11

      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine indtrig (first_line, last_line)
c
c --- this routine indexes a trigonal cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      logical extinct
      integer hend,kend,lend
      integer first_line,last_line
      integer h2k2,l2,old_h2k2,old_l2

      do 1 i = first_line, last_line
         hend = nint(sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         old_h2k2 = 0
         old_l2 = 0

         do h = 0,hend
            kend = h
            if (ikset .eq. 1) then
               kend = min(kend,ikmax)
            endif

            do k = 0,kend
               h2k2 = h*h+k*k
               rest = (qobs(i)+qdel(i)-real(h2k2)*g11) / g33
               if (rest .gt. 0.0) then
                  lend = nint(sqrt(rest))
               else
                  lend = 0
               endif
               if (ilset .eq. 1) then
                  lend = min(lend,ilmax)
               endif

               do l = 0,lend
                  l2 = l*l
                  if (.not.extinct(h,k,l)) then		
                     qtest = real(h2k2)*g11+real(l2)*g33
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (h2k2 .ne. old_h2k2 .or.
     >                      l2 .ne. old_l2) then
                           old_h2k2 = h2k2
                           old_l2 = l2
                           if (absdel .lt. bestdel) then
                              bestdel = absdel
                              hbest = h
                              kbest = k
                              lbest = l
                           endif
                           nokay(i) = nokay(i)+1
                           if (iverb .eq. 1) then
                              call write_line(ludisp,lulog,ilogfl,
     >                        i,h,k,l,qobs(i),qtest,wave)
                           endif
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = (ih(i)*ih(i)+ik(i)*ik(i))*g11+il(i)*il(i)*g33
    1 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine indtet (first_line, last_line)
c
c --- this routine indexes a tetragonal cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      logical extinct
      integer hend,kend,lend
      integer first_line,last_line
      integer h2k2,l2,old_h2k2,old_l2

      do 1 i = first_line, last_line
         hend = nint(sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         old_h2k2 = 0
         old_l2 = 0

         do h = 0,hend
            kend = h
            if (ikset .eq. 1) then
               kend = min(kend,ikmax)
            endif

            do k = 0,kend
               h2k2 = h*h+k*k
               rest = (qobs(i)+qdel(i)-real(h2k2)*g11) / g33
               if (rest .gt. 0.0) then
                  lend = nint(sqrt(rest))
               else
                  lend = 0
               endif
               if (ilset .eq. 1) then
                  lend = min(lend,ilmax)
               endif

               do l = 0,lend
                  l2 = l*l
                  if (.not.extinct(h,k,l)) then		
                     qtest = real(h2k2)*g11+real(l2)*g33
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (h2k2 .ne. old_h2k2 .or.
     >                      l2 .ne. old_l2) then
                           old_h2k2 = h2k2
                           old_l2 = l2
                           if (absdel .lt. bestdel) then
                              bestdel = absdel
                              hbest = h
                              kbest = k
                              lbest = l
                           endif
                           nokay(i) = nokay(i)+1
                           if (iverb .eq. 1) then
                              call write_line(ludisp,lulog,ilogfl,
     >                        i,h,k,l,qobs(i),qtest,wave)
                           endif
                        endif
                     endif
                  endif
               enddo
            enddo
         enddo
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = (ih(i)*ih(i)+ik(i)*ik(i))*g11+il(i)*il(i)*g33
    1 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine indhex (first_line, last_line)
c
c --- this routine indexes a hexagonal cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      logical extinct
      integer hend,kend,lend
      integer first_line,last_line
      integer h2hkk2,l2,old_h2hkk2,old_l2

      do 1 i = first_line, last_line
         hend = nint(sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         old_h2hkk2 = 0
         old_l2 = 0
         do 2 h = 0,hend
            kend = h
            if (ikset .eq. 1) then
               kend = min(kend,ikmax)
            endif
            do 3 k = 0,kend
               h2hkk2 = h*h+h*k+k*k
               rest = (qobs(i)+qdel(i)-real(h2hkk2)*g11) / g33
               if (rest .gt. 0.0) then
                  lend = nint(sqrt(rest))
               else
                  lend = 0
               endif
               if (ilset .eq. 1) then
                  lend = min(lend,ilmax)
               endif
               do 4 l = 0,lend
                  if (.not.extinct(h,k,l)) then		
                     l2 = l*l
                     qtest = real(h2hkk2)*g11+real(l2)*g33
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (h2hkk2 .ne. old_h2hkk2 .or.
     >                      l2 .ne. old_l2) then
                           old_h2hkk2 = h2hkk2
                           old_l2 = l2
                           if (absdel .lt. bestdel) then
                              bestdel = absdel
                              hbest = h
                              kbest = k
                              lbest = l
                           endif
                           nokay(i) = nokay(i)+1
                           if (iverb .eq. 1) then
                              call write_line(ludisp,lulog,ilogfl,
     >                        i,h,k,l,qobs(i),qtest,wave)
                           endif
                        endif
                     endif
                  endif
    4          continue
    3       continue
    2    continue
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = (ih(i)*ih(i)+ih(i)*ik(i)+ik(i)*ik(i))*g11 +
     >                 il(i)*il(i)*g33
    1 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine indort (first_line, last_line)
c
c --- this routine indexes an ortorhombic cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      logical extinct
      integer hend,kend,lend
      integer first_line,last_line

      do 1 i = first_line, last_line
         hend = nint(sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         do 2 h = 0,hend
            rh2g11 = real(h*h)*g11
            rest = (qobs(i)+qdel(i)-rh2g11) / g22
            if (rest .gt. 0.0) then
               kend = nint(sqrt(rest))
            else
               kend = 0
            endif
            if (ikset .eq. 1) then
               kend = min(kend,ikmax)
            endif
            do 3 k = 0,kend
               rk2g22 = real(k*k)*g22
               rest = (qobs(i)+qdel(i)-rh2g11-rk2g22) / g33
               if (rest .gt. 0.0) then
                  lend = nint(sqrt(rest))
               else
                  lend = 0
               endif
               if (ilset .eq. 1) then
                  lend = min(lend,ilmax)
               endif
               do 4 l = 0,lend
                  if (.not.extinct(h,k,l)) then		
                     qtest = rh2g11+rk2g22+l*l*g33
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (absdel .lt. bestdel) then
                           bestdel = absdel
                           hbest = h
                           kbest = k
                           lbest = l
                        endif
                        nokay(i) = nokay(i)+1
                        if (iverb .eq. 1) then
                           call write_line(ludisp,lulog,ilogfl,
     >                     i,h,k,l,qobs(i),qtest,wave)
                        endif
                     endif
                  endif
    4          continue
    3       continue
    2    continue
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = ih(i)*ih(i)*g11+ik(i)*ik(i)*g22+il(i)*il(i)*g33
    1 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine indmon (first_line, last_line)
c
c --- this routine indexes a monoclinic cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      logical extinct
      integer lbeg,hend,kend,lend
      integer first_line,last_line

      do 1 i = first_line, last_line
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         hend = nint(1.42*sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hend = min(hend,ihmax)
         endif
         kend = nint(sqrt((qobs(i)+qdel(i))/g22))
         if (ikset .eq. 1) then
            kend = min(kend,ikmax)
         endif
         lend = nint(1.42*sqrt((qobs(i)+qdel(i))/g33))
         if (ilset .eq. 1) then
            lend = min(lend,ilmax)
         endif
         do 2 h = 0,hend
            do 3 k = 0,kend
               if (h .eq. 0) then
                  lbeg = 0
               else
                  lbeg = -lend
                  if (ilset .eq. 1) then
                     lbeg = max(lbeg,ilmin)
                  endif
               endif
               do 4 l = lbeg,lend
                  if (.not.extinct(h,k,l)) then		
                     qtest = real(h*h)*g11+real(k*k)*g22+
     >                        real(l*l)*g33+real(h*l)*g13
                     absdel = abs(qtest-qobs(i))
                     if (absdel .lt. qdel(i)) then
                        if (absdel .lt. bestdel) then
                           bestdel = absdel
                           hbest = h
                           kbest = k
                           lbest = l
                        endif
                        nokay(i) = nokay(i)+1
                        if (iverb .eq. 1) then
                           call write_line(ludisp,lulog,ilogfl,
     >                     i,h,k,l,qobs(i),qtest,wave)
                        endif
                     endif
                  endif
    4          continue
    3       continue
    2    continue
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = ih(i)*ih(i)*g11 + ik(i)*ik(i)*g22 +
     >                il(i)*il(i)*g33 + ih(i)*il(i)*g13
    1 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine indtric (first_line, last_line)
c
c --- this routine indexes a triclinic cell.
c
      include 'puder.cmn'

      integer h,k,l,hbest,kbest,lbest
      integer hbeg,kbeg,lbeg,hend,kend,lend
      integer first_line,last_line

      do 1 i = first_line, last_line
         hbest = 0
         kbest = 0
         lbest = 0
         qtest = 0.0
         bestdel = 1.0
         nokay(i) = 0
         hbeg = 0
         hend = nint(1.42*sqrt((qobs(i)+qdel(i))/g11))
         if (ihset .eq. 1) then
            hbeg = max(hbeg,ihmin)
            hend = min(hend,ihmax)
         endif
         kend = nint(1.42*sqrt((qobs(i)+qdel(i))/g22))
         if (ikset .eq. 1) then
            kend = min(kend,ikmax)
         endif
         lend = nint(1.42*sqrt((qobs(i)+qdel(i))/g33))
         if (ilset .eq. 1) then
            lend = min(lend,ilmax)
         endif
c
c ------ loop over H, and compute limits of K.
c
         do 2 h = hbeg,hend
            if (h .eq. 0) then
               kbeg = 0
            else
               kbeg = -kend
               if (ikset .eq. 1) then
                  kbeg = max(kbeg,ikmin)
               endif
            endif
c
c --------- loop over K, and compute limits of L.
c
            do 3 k = kbeg,kend
               if (h .eq. 0 .and. k .eq. 0) then
                  lbeg = 0
               else
                  lbeg = -lend
                  if (ilset .eq. 1) then
                     lbeg = max(lbeg,ilmin)
                  endif
               endif
c
c ------------ loop over L.
c
               do 4 l = lbeg,lend
                  qtest = real(h*h)*g11+real(k*k)*g22+real(l*l)*g33+
     >                     real(h*k)*g12+real(h*l)*g13+real(k*l)*g23
                  absdel = abs(qtest-qobs(i))
                  if (absdel .lt. qdel(i)) then
                        if (absdel .lt. bestdel) then
                           bestdel = absdel
                           hbest = h
                           kbest = k
                           lbest = l
                        endif
                     nokay(i) = nokay(i)+1
                     if (iverb .eq. 1) then
                        call write_line(ludisp,lulog,ilogfl,
     >                  i,h,k,l,qobs(i),qtest,wave)
                     endif
                  endif
    4          continue
    3       continue
    2    continue
         if (nokay(i) .eq. 0 .and. iverb .eq. 1) then
            call write_not_ind_line (ludisp,lulog,ilogfl,i,qobs(i),wave)
         endif
         if (i .lt. last_line .and. iverb .eq. 1) then
            call write_line_separator (ludisp,lulog,ilogfl)
         endif
         if (ilock(i) .eq. 0) then
            ih(i) = hbest
            ik(i) = kbest
            il(i) = lbest
         endif
         qcalc(i) = real(ih(i)*ih(i))*g11 + real(ik(i)*ik(i))*g22 +
     >              real(il(i)*il(i))*g33 + real(ih(i)*ik(i))*g12 +
     >              real(ih(i)*il(i))*g13 + real(ik(i)*il(i))*g23
    1 continue

      return
      end

c-----------------------------------------------------------------------

      logical function even(i)
      if (mod(i,2) .eq. 0) then
         even = .true.
      else
         even = .false.
      endif
      return
      end

c-----------------------------------------------------------------------

      logical function odd(i)
      if (mod(i,2) .ne. 0) then
         odd = .true.
      else
         odd = .false.
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine exttop (lukeys,ludisp,numext,exsymb,ilatt,str)
c
      integer    exsymb(50,5)
      character  cmd*80, str*80
      logical    flag

      if (str .ne. ' ') then
         flag = .true.
      else
         flag = .false.
      endif

 1    continue
      if (.not. flag) then
         write (ludisp,'(a,$)')   ' Puder.Cond>'
         read  (lukeys,'(a)') str
         if (str .eq. ' ') then
            goto 1
         endif
      endif

      call strip(str)
      call move_first_element (str,cmd)
      call upline(cmd)

      if (cmd .eq. 'ANALYSE' .or.
     >    cmd .eq. 'ANALYS' .or.
     >    cmd .eq. 'ANALY' .or.
     >    cmd .eq. 'ANAL' .or.
     >    cmd .eq. 'ANA' .or.
     >    cmd .eq. 'AN' ) then
         call extana (str)

      elseif (cmd .eq. 'ENTER' .or.
     >        cmd .eq. 'ENTE' .or.
     >        cmd .eq. 'ENT' .or.
     >        cmd .eq. 'EN' .or.
     >        cmd .eq. 'INPUT' .or.
     >        cmd .eq. 'INPU' .or.
     >        cmd .eq. 'INP' .or.
     >        cmd .eq. 'IN'  ) then
         call extinp (lukeys,ludisp,numext,exsymb,ilatt,str)

      elseif (cmd .eq. 'DELETE' .or.
     >        cmd .eq. 'DELET' .or.
     >        cmd .eq. 'DELE' .or.
     >        cmd .eq. 'DEL' .or.
     >        cmd .eq. 'DE' ) then
         call extdel (lukeys,ludisp,numext,exsymb,str)

      elseif (cmd .eq. 'LIST' .or.
     >        cmd .eq. 'LIS' .or.
     >        cmd .eq. 'LI') then
         call extlis (ludisp,numext,exsymb,0)

      elseif (cmd .eq. 'INFO' .or.
     >        cmd .eq. 'INF') then
         call extinf (ludisp)

      elseif (cmd .eq. 'EXIT' .or.
     >        cmd .eq. 'EXI' .or.
     >        cmd .eq. 'EX' ) then
         return

      elseif (cmd .eq. 'HELP' .or.
     >        cmd .eq. 'HEL' .or.
     >        cmd .eq. 'HE' ) then
         call exthlp (ludisp)

      else
         write (ludisp,'(a)') ' Unknown command.'

      endif
      if (.not.flag) then
         goto 1
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine extana (str)
c
c --- This routine analyses the list of reflexions with respect to
c     certain reflexion conditions for some strict subclasses of 
c     reflexion types, and also with respect to a possible
c     lattice centering ...
c
      include 'puder.cmn'

      character  line*80, str*80, command*80
      logical    flag, even, odd

 1    continue
      if (str .ne. ' ') then
         line = str
         flag = .true.
      else
         write (ludisp,'(a,$)')   ' Puder.Cond.Analyse>'
         read  (lukeys,'(a)') line
         flag = .false.
      endif
c
c --- decode the given analysing command !
c
      ip = index(line,':')
      if (ip .ne. 0) then
         line(ip:ip) = ' '
      endif
      if (line .eq. ' ') then
         goto 1
      endif
      call upline (line)
      call strip (line)      
      call move_first_element (line,command)
c
c --- Take the appropriate action to each command given ...
c
      if (command .eq. 'HELP' .or.
     >    command .eq. 'HEL' .or.
     >    command .eq. 'HE' ) then
         write (ludisp,
     >   '(/,a,a,/,a,/,a,/,a,/,a,/,a,/,a,/a,/,a,/)'
     >         )
     >   ' An analysing command should be entered in one line and',
     >   ' should be either:',
     >   ' ',
     >   '  1)   A reflexion type.',
     >   '       ( H00, 0K0, 00L, HK0, H0L, 0KL, HHL or HH0 )',
     >   ' ',
     >   '  2)   A lattice centering code.',
     >   '       ( P, F, I, A, B or C )',
     >   ' ',
     >   ' or one of the following commands.'
         write(ludisp,'(a)')
     >   ' EXIT .....: Exit from analyser mode.'
         write(ludisp,'(a)')
     >   ' INDEX ....: Index some or all lines.'
         write(ludisp,'(a)')
     >   ' INFO .....: Get information on available extinctions.'
         write(ludisp,'(a)')
     >   ' VERBOSE ..: Toggle verbose flag.'
         write(ludisp,'(a)') ' '

      elseif (command .eq. 'EXIT' .or.
     >        command .eq. 'EXI' .or.
     >        command .eq. 'EX' ) then
         return

      elseif (command .eq. 'INDEX' .or.
     >        command .eq. 'INDE' .or.
     >        command .eq. 'IND' .or.
     >        command .eq. 'IN' ) then
         call indlin (line)

      elseif (command .eq. 'VERBOSE' .or.
     >        command .eq. 'VERBOS' .or.
     >        command .eq. 'VERBO' .or.
     >        command .eq. 'VERB' .or.
     >        command .eq. 'VER' .or.
     >        command .eq. 'VE' ) then
         if (iverb .eq. 1) then
            iverb = 0
            write(ludisp,'(a)')
     >      ' No verbose printing in the indexing phase.'
         else
            iverb = 1
            write(ludisp,'(a)')
     >      ' Verbose printing in the indexing phase.'
         endif
c
c ------ check first if a lattice centering symbol was given.
c
      elseif (command .eq. 'P') then
         write (ludisp,'(a)') 
     >   ' Any lattice (even this data) could be regarded as primitive.'

      elseif (command .eq. 'F') then
         not_valid = 0
         do i = 1,nobs
            if (.not.even(ih(i)+ik(i)) .or.
     >          .not.even(ih(i)+il(i)) .or.
     >          .not.even(ik(i)+il(i)) ) then
               not_valid = not_valid+1
               call hkl_not_valid
     >         (iverb,ludisp,i,ih(i),ik(i),il(i))
            endif
         enddo
         if (not_valid .eq. 0) then
            write (ludisp,'(a)') ' Lattice could be F-centered.'
         else
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid,' of ',nobs,
     >      ' reflexions breaking F-centering condition.'
         endif

      elseif (command .eq. 'I') then
         not_valid = 0
         do i = 1,nobs
            if (.not.even(ih(i)+ik(i)+il(i)) ) then
               not_valid = not_valid+1
               call hkl_not_valid
     >         (iverb,ludisp,i,ih(i),ik(i),il(i))
            endif
         enddo
         if (not_valid .eq. 0) then
            write (ludisp,'(a)') ' Lattice could be I-centered.'
         else
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid,' of ',nobs,
     >      ' reflexions breaking I-centering condition.'
         endif

      elseif (command .eq. 'A') then
         not_valid = 0
         do i = 1,nobs
            if (.not.even(ik(i)+il(i)) ) then
               not_valid = not_valid+1
               call hkl_not_valid
     >         (iverb,ludisp,i,ih(i),ik(i),il(i))
            endif
         enddo
         if (not_valid .eq. 0) then
            write (ludisp,'(a)') ' Lattice could be A-centered.'
         else
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid,' of ',nobs,
     >      ' reflexions breaking A-centering condition.'
         endif

      elseif (command .eq. 'B') then
         not_valid = 0
         do i = 1,nobs
            if (.not.even(ih(i)+il(i)) ) then
               not_valid = not_valid+1
               call hkl_not_valid
     >         (iverb,ludisp,i,ih(i),ik(i),il(i))
            endif
         enddo
         if (not_valid .eq. 0) then
            write (ludisp,'(a)') ' Lattice could be B-centered.'
         else
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid,' of ',nobs,
     >      ' reflexions breaking B-centering condition.'
         endif

      elseif (command .eq. 'C') then
         not_valid = 0
         do i = 1,nobs
            if (even(ih(i)+ik(i)) ) then
               not_valid = not_valid+1
               call hkl_not_valid
     >         (iverb,ludisp,i,ih(i),ik(i),il(i))
            endif
         enddo
         if (not_valid .eq. 0) then
            write (ludisp,'(a)') ' Lattice could be C-centered.'
         else
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid,' of ',nobs,
     >      ' reflexions breaking C-centering condition.'
         endif
c
c --- check HK0 reflexions.
c
      elseif (command .eq. 'HK0') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'H=2N' .and.
     >       line .ne. 'K=2N' .and.
     >       line .ne. 'H+K=2N' .and.
     >       line .ne. 'H+K=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'H=2N') then
            not_valid1 = 0
            nhk0 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  nhk0 = nhk0+1
                  if (odd(ih(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',nhk0,
     >      ' HK0 reflexions breaking H=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'K=2N') then
            not_valid2 = 0
            nhk0 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  nhk0 = nhk0+1
                  if (odd(ik(i))) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',nhk0,
     >      ' HK0 reflexions breaking K=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'H+K=2N') then
            not_valid3 = 0
            nhk0 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  nhk0 = nhk0+1
                  if (odd(ih(i)+ik(i))) then
                     not_valid3 = not_valid3+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid3,' of ',nhk0,
     >      ' HK0 reflexions breaking H+K=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'H+K=4N') then
            not_valid4 = 0
            nhk0 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  nhk0 = nhk0+1
                  if (mod(ih(i)+ik(i),4) .ne. 0) then
                     not_valid4 = not_valid4+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid4,' of ',nhk0,
     >      ' HK0 reflexions breaking H+K=4N condition.'
         endif
c
c --- check H0L reflexions.
c
      elseif (command .eq. 'H0L') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'H=2N' .and.
     >       line .ne. 'L=2N' .and.
     >       line .ne. 'H+L=2N' .and.
     >       line .ne. 'H+L=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'H=2N') then
            not_valid1 = 0
            nh0l = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  nh0l = nh0l+1
                  if (odd(ih(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',nh0l,
     >      ' H0L reflexions breaking H=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'L=2N') then
            not_valid2 = 0
            nh0l = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  nh0l = nh0l+1
                  if (odd(il(i))) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',nh0l,
     >      ' H0L reflexions breaking L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'H+L=2N') then
            not_valid3 = 0
            nh0l = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  nh0l = nh0l+1
                  if (odd(ih(i)+il(i))) then
                     not_valid3 = not_valid3+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid3,' of ',nh0l,
     >      ' H0L reflexions breaking H+L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'H+L=4N') then
            not_valid4 = 0
            nh0l = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  nh0l = nh0l+1
                  if (mod(ih(i)+il(i),4) .ne. 0) then
                     not_valid4 = not_valid4+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid4,' of ',nh0l,
     >      ' H0L reflexions breaking H+L=4N condition.'
         endif
c
c --- check 0KL reflexions.
c
      elseif (command .eq. '0KL') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'K=2N' .and.
     >       line .ne. 'L=2N' .and.
     >       line .ne. 'K+L=2N' .and.
     >       line .ne. 'K+L=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'K=2N') then
            not_valid1 = 0
            n0kl = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).ne.0 ) then
                  n0kl = n0kl+1
                  if (odd(ik(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',n0kl,
     >      ' 0KL reflexions breaking K=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'L=2N') then
            not_valid2 = 0
            n0kl = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).ne.0 ) then
                  n0kl = n0kl+1
                  if (odd(il(i))) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',n0kl,
     >      ' 0KL reflexions breaking L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'K+L=2N') then
            not_valid3 = 0
            n0kl = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).ne.0 ) then
                  n0kl = n0kl+1
                  if (odd(ik(i)+il(i))) then
                     not_valid3 = not_valid3+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid3,' of ',n0kl,
     >      ' 0KL reflexions breaking K+L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'K+L=4N') then
            not_valid4 = 0
            n0kl = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).ne.0 ) then
                  n0kl = n0kl+1
                  if (mod(ik(i)+il(i),4) .ne. 0) then
                     not_valid4 = not_valid4+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid4,' of ',n0kl,
     >      ' 0KL reflexions breaking K+L=4N condition.'
         endif
c
c --- check HHL reflexions.
c
      elseif (command .eq. 'HHL') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'L=2N' .and.
     >       line .ne. '2H+L=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'L=2N') then
            not_valid1 = 0
            nhhl = 0
            do i = 1,nobs
               if (ih(i).eq.ik(i).and.ik(i).ne.0.and.il(i).ne.0) then
                  nhhl = nhhl+1
                  if (odd(il(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',nhhl,
     >      ' HHL reflexions breaking L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. '2H+L=4N') then
            not_valid2 = 0
            nhhl = 0
            do i = 1,nobs
               if (ih(i).eq.ik(i).and.ik(i).ne.0.and.il(i).ne.0) then
                  nhhl = nhhl+1
                  if (mod(2*ih(i)+il(i),4) .ne. 0) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',nhhl,
     >      ' HHL reflexions breaking 2H+L=4N condition.'
         endif
c
c --- check H00 reflexions.
c
      elseif (command .eq. 'H00') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'H=2N' .and.
     >       line .ne. 'H=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'H=2N') then
            not_valid1 = 0
            nh00 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).eq.0 ) then
                  nh00 = nh00+1
                  if (odd(ih(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',nh00,
     >      ' H00 reflexions breaking H=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'H=4N') then
            not_valid2 = 0
            nh00 = 0
            do i = 1,nobs
               if (ih(i).ne.0 .and. ik(i).eq.0 .and. il(i).eq.0 ) then
                  nh00 = nh00+1
                  if (mod(ih(i),4) .ne. 0) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',nh00,
     >      ' H00 reflexions breaking H=4N condition.'
         endif
c
c --- check 0K0 reflexions.
c
      elseif (command .eq. '0K0') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'K=2N' .and.
     >       line .ne. 'K=4N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'K=2N') then
            not_valid1 = 0
            n0k0 = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  n0k0 = n0k0+1
                  if (odd(ik(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',n0k0,
     >      ' 0K0 reflexions breaking K=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'K=4N') then
            not_valid2 = 0
            n0k0 = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).ne.0 .and. il(i).eq.0 ) then
                  n0k0 = n0k0+1
                  if (mod(ik(i),4) .ne. 0) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',n0k0,
     >      ' 0K0 reflexions breaking K=4N condition.'
         endif
c
c --- check 00L reflexions.
c
      elseif (command .eq. '00L') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'L=2N' .and.
     >       line .ne. 'L=3N' .and.
     >       line .ne. 'L=4N' .and.
     >       line .ne. 'L=6N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         if (line .eq. ' ' .or. line .eq. 'L=2N') then
            not_valid1 = 0
            n00l = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  n00l = n00l+1
                  if (odd(il(i))) then
                     not_valid1 = not_valid1+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid1,' of ',n00l,
     >      ' 00L reflexions breaking L=2N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'L=3N') then
            not_valid2 = 0
            n00l = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  n00l = n00l+1
                  if (mod(il(i),3) .ne. 0) then
                     not_valid2 = not_valid2+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid2,' of ',n00l,
     >      ' 00L reflexions breaking L=3N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'L=4N') then
            not_valid3 = 0
            n00l = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  n00l = n00l+1
                  if (mod(il(i),4) .ne. 0) then
                     not_valid3 = not_valid3+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid3,' of ',n00l,
     >      ' 00L reflexions breaking L=4N condition.'
         endif

         if (line .eq. ' ' .or. line .eq. 'L=6N') then
            not_valid4 = 0
            n00l = 0
            do i = 1,nobs
               if (ih(i).eq.0 .and. ik(i).eq.0 .and. il(i).ne.0 ) then
                  n00l = n00l+1
                  if (mod(il(i),6) .ne. 0) then
                     not_valid4 = not_valid4+1
                     call hkl_not_valid
     >               (iverb,ludisp,i,ih(i),ik(i),il(i))
                  endif
               endif
            enddo
            write (ludisp,'(i4,a,i4,a)') 
     >      not_valid4,' of ',n00l,
     >      ' 00L reflexions breaking L=6N condition.'
         endif
c
c --- check HH0 reflexions.
c
      elseif (command .eq. 'HH0') then
         if (line .ne. ' ' .and. 
     >       line .ne. 'H=2N' ) then
            write(ludisp,'(a)') ' Unknown condition type ...'
            goto 1
         endif

         not_valid1 = 0
         nhh0 = 0
         do i = 1,nobs
            if (ih(i).eq.ik(i) .and. ik(i).ne.0 .and. il(i).eq.0 ) then
               nhh0 = nhh0+1
               if (odd(ih(i))) then
                  not_valid1 = not_valid1+1
                  call hkl_not_valid
     >            (iverb,ludisp,i,ih(i),ik(i),il(i))
               endif
            endif
         enddo
         write (ludisp,'(i4,a,i4,a)') 
     >   not_valid1,' of ',nhh0,
     >   ' HH0 reflexions breaking H=2N condition.'

      else
         write (ludisp,'(a,a,a)')
     > ' Unknown analysing command /',command(1:lastch(command)),'/'

      endif

      if (flag) then
         return
      else
         goto 1
      endif

      end

c-----------------------------------------------------------------------

      subroutine hkl_not_valid (iverb,ludisp,i,h,k,l)
      integer h,k,l
      if (iverb .eq. 1) then
         write(ludisp,'(4i5,a)') 
     >   i,h,k,l,' breaking reflexion condition...'
      endif
      return
      end

c-----------------------------------------------------------------------

      subroutine extlis (ludev,numext,exsymb,itypflag)
c
c --- this routine list the reflexion conditions saved in the array
c     "EXSYMB"
c
      integer    exsymb(50,5)
      character  str*80

      do i=1,numext

         str = ' '
         if (exsymb(i,5) .eq. 1) then
            str(1:7) = 'hk0  : '
         elseif (exsymb(i,5) .eq. 2) then
            str(1:7) = 'h0l  : '
         elseif (exsymb(i,5) .eq. 3) then
            str(1:7) = '0kl  : '
         elseif (exsymb(i,5) .eq. 4) then
            str(1:7) = 'hhl  : '
         elseif (exsymb(i,5) .eq. 5) then
            str(1:7) = 'h00  : '
         elseif (exsymb(i,5) .eq. 6) then
            str(1:7) = '0k0  : '
         elseif (exsymb(i,5) .eq. 7) then
            str(1:7) = '00l  : '
         elseif (exsymb(i,5) .eq. 8) then
            str(1:7) = 'hh0  : '
         elseif (exsymb(i,5) .eq. 9) then
            str(1:7) = 'h-hl : '
         endif

         iptr=8
         nprint = 0

         if (exsymb(i,1) .eq. 1) then
            str(iptr:iptr) = 'h'
            iptr = iptr+1
            nprint = nprint+1
         elseif (exsymb(i,1) .eq. 2) then
            str(iptr:iptr+1) = '2h'
            iptr = iptr+2
            nprint = nprint+1
         endif

         if (exsymb(i,2) .eq. 1) then
            if (nprint .gt. 0 ) then
               str(iptr:iptr) = '+'
               iptr = iptr+1
            endif
            str(iptr:iptr) = 'k'
            iptr = iptr+1
            nprint = nprint+1
         elseif (exsymb(i,2) .eq. 2) then
            if (nprint .gt. 0 ) then
               str(iptr:iptr) = '+'
               iptr = iptr+1
            endif
            str(iptr:iptr+1) = '2k'
            iptr = iptr+2
            nprint = nprint+1
         endif

         if (exsymb(i,3) .eq. 1) then
            if (nprint .gt. 0 ) then
               str(iptr:iptr) = '+'
               iptr = iptr+1
            endif
            str(iptr:iptr) = 'l'
            iptr = iptr+1
            nprint = nprint+1
         elseif (exsymb(i,3) .eq. 2) then
            if (nprint .gt. 0 ) then
               str(iptr:iptr) = '+'
               iptr = iptr+1
            endif
            str(iptr:iptr+1) = '2l'
            iptr = iptr+2
            nprint = nprint+1
         endif

         str(iptr:iptr+2) = ' = '
         iptr=iptr+3

         if (exsymb(i,4) .eq. 2) then
            str(iptr:iptr+1) = '2n'
         elseif (exsymb(i,4) .eq. 3) then
            str(iptr:iptr+1) = '3n'
         elseif (exsymb(i,4) .eq. 4) then
            str(iptr:iptr+1) = '4n'
         elseif (exsymb(i,4) .eq. 6) then
            str(iptr:iptr+1) = '6n'
         endif

         if (itypflag .eq. 1) then
            write (ludev,'(a,a)') 'Condition Input ',str(1:lastch(str))
         else
            write (ludev,'(a,i2,a,a)') ' #',i,'  ',str(1:lastch(str))
         endif

      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine extinp (lukeys,ludisp,numext,exsymb,ilatt,str)
c
c --- This routine decodes extiction conditions and stores the
c     codes of these to be used in other routines in the
c     integer array EXSYM (i,j)
c
c     The first index is only an ordinal number of the extinctions.
c     (There is NUMEXT of them ...)
c
c     EXSYMB (i,1) = #H
c     EXSYMB (i,2) = #K
c     EXSYMB (i,3) = #L
c     EXSYMB (i,4) = modulus number
c     EXSYMB (i,5) = reflexion type code.
c
      integer    exsymb(50,5)
      character  line*80,code*80,reftyp*10,str*80
      logical    flag

 1    continue
      if (str .ne. ' ') then
         line = str
         flag = .true.
      else
         write (ludisp,'(a,$)')   ' Puder.Cond.Input>'
         read  (lukeys,'(a)') line
         flag = .false.
      endif
      if (line .eq. ' ') then
         goto 1
      endif
c
c --- decode the given reflection code !
c
      call upline (line)
      call strip (line)      

      if (line .eq. 'HELP' .or.
     >    line .eq. 'HEL' .or.
     >    line .eq. 'HE' ) then
         write (ludisp,'(/,a,a,/,a,//,a,//,a,/,a,//,a,/)')
     >   ' A reflection condition should be entered in one line and',
     >   ' should contain:',
     >   ' a reflexion type, a colon ":" and the actual condition.',
     >   ' Example:   0K0 : K=2N',
     >   ' Lattice centering conditions could also be entered.',
     >   ' Available codes are: P, F, I, A, B or C',
     >   ' End input mode with the command EXIT.'
         goto 1

      elseif (line .eq. 'EXIT' .or.
     >        line .eq. 'EXI' .or.
     >        line .eq. 'EX' ) then
         return
c
c ------ check first if a lattice centering symbol was given.
c
      elseif (line .eq. 'P') then
         ilatt = 1
         write (ludisp,'(a)') ' Primitve lattices used.'

      elseif (line .eq. 'F') then
         ilatt = 2
         write (ludisp,'(a)') ' F-centered lattice used.'

      elseif (line .eq. 'I') then
         ilatt = 3
         write (ludisp,'(a)') ' I-centered lattice used.'

      elseif (line .eq. 'A') then
         ilatt = 4
         write (ludisp,'(a)') ' A-centered lattice used.'

      elseif (line .eq. 'B') then
         ilatt = 5
         write (ludisp,'(a)') ' B-centered lattice used.'

      elseif (line .eq. 'C') then
         ilatt = 6
         write (ludisp,'(a)') ' C-centered lattice used.'

      elseif (line .eq. 'R') then
         ilatt = 7
         write (ludisp,'(a)') ' R-centered lattice used.'

      else
c
c ------ not lattice centring, but another type of symbol
c
         code = ' '
         j = 1
         do 2 i = 1,lastch(line)
            if (line(i:i) .ne. ' ') then
               code (j:j) = line(i:i)
               j = j+1
            endif
 2       continue
c
c ------ now string "code" contains REF-TYPE, :, REFLECTION-CONDITION 
c        for example: 0K0:K=2N
c
         reftyp = ' '
         i = index(code,':')
         if (i .eq. 0) then
            write (ludisp,'(a)') 
     >      ' Cannot decode this string, please try again...'
            if (.not.flag) then
               goto 1
            endif
         else
            reftyp = code(1:i-1)
            code (1:i) = ' '
         endif

         call strip (code)
c
c ------ check hk0 reflexions.
c
         if (reftyp .eq. 'HK0') then
           if (code .eq. 'H=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 1
            elseif (code .eq. 'K=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 1
            elseif (code .eq. 'H+K=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 1
            elseif (code .eq. 'H+K=4N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 4
               exsymb(numext,5) = 1
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type HK0.'
            endif
c
c ------ check h0l reflexions.
c
         elseif (reftyp .eq. 'H0L') then
            if (code .eq. 'H=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 2
            elseif (code .eq. 'L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 2
            elseif (code .eq. 'H+L=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 2
            elseif (code .eq. 'H+L=4N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 4
               exsymb(numext,5) = 2
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type H0L.'
            endif
c
c ------ check 0kl reflexions.
c
      elseif (reftyp .eq. '0KL') then
         if    (code .eq. 'K=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 3
            elseif (code .eq. 'L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 3
            elseif (code .eq. 'K+L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 3
            elseif (code .eq. 'K+L=4N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 1
               exsymb(numext,4) = 4
               exsymb(numext,5) = 3
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type 0KL.'
            endif
c
c ------ check hhl reflexions.
c
         elseif (reftyp .eq. 'HHL') then
            if (code .eq. 'L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 4
            elseif (code .eq. 'L=3N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 3
               exsymb(numext,5) = 4
            elseif (code .eq. '2H+L=4N') then
               numext = numext+1
               exsymb(numext,1) = 2
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 4
               exsymb(numext,5) = 4
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type HHL.'
            endif
c
c ------ check h00 reflexions.
c
         elseif (reftyp .eq. 'H00') then
            if (code .eq. 'H=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 5
            elseif (code .eq. 'H=4N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 5
            else
               write (ludisp,'(a)')
     >        ' Unknown reflection code for reflexion type H00.'
            endif
c
c ------ check 0k0 reflexions.
c
         elseif (reftyp .eq. '0K0') then
            if (code .eq. 'K=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 6
            elseif (code .eq. 'K=4N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 1
               exsymb(numext,3) = 0
               exsymb(numext,4) = 4
               exsymb(numext,5) = 6
            else
               write (ludisp,'(a)')
     >        ' Unknown reflection code for reflexion type 0K0.'
            endif
c
c ------ check 00l reflexions.
c
         elseif (reftyp .eq. '00L') then
            if (code .eq. 'L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 7
            elseif (code .eq. 'L=3N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 3
               exsymb(numext,5) = 7
            elseif (code .eq. 'L=4N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 4
               exsymb(numext,5) = 7
            elseif (code .eq. 'L=6N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 6
               exsymb(numext,5) = 7
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type 00L.'
            endif
c
c ------ check hh0 reflexions.
c
         elseif (reftyp .eq. 'HH0') then
            if (code .eq. 'H=2N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 0
               exsymb(numext,4) = 2
               exsymb(numext,5) = 8
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type HH0.'
            endif
c
c ------ check H-HL reflexions.
c
         elseif (reftyp .eq. 'H-HL') then
            if (code .eq. 'H+L=3N') then
               numext = numext+1
               exsymb(numext,1) = 1
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 3
               exsymb(numext,5) = 9
            elseif (code .eq. 'L=2N') then
               numext = numext+1
               exsymb(numext,1) = 0
               exsymb(numext,2) = 0
               exsymb(numext,3) = 1
               exsymb(numext,4) = 2
               exsymb(numext,5) = 9
            else
               write (ludisp,'(a)')
     >         ' Unknown reflection code for reflexion type H-HL.'
            endif

         elseif (reftyp .eq. 'HKL') then
            write (ludisp,'(a,a)') ' Use the LATT command to specify',
     >                             ' conditions for general reflexions.'

         else
            write (ludisp,'(a,a)') ' Unknown reflexion type: ',reftyp

         endif

      endif

      if (.not.flag) then
         goto 1
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine extinf (ludisp)
c
c --- this routine gives some information about the use of extinction
c     conditions, more specific it lists the allowed "codes".
c
      write (ludisp,'(a)') ' '
      write (ludisp,'(a,a)')
     >' A reflexion condition consists of a reflexion type,',
     >' to indicate when'
      write (ludisp,'(a,a)')
     >' the reflexion condition is applicable, a colon ":" and',
     >' finally the '
      write (ludisp,'(a)')
     >' actual reflexion condition.'
      write (ludisp,'(a)')
     >' A list of the allowed reflection conditions is given below.'
      write (ludisp,'(a)')
     >' '
      write (ludisp,'(a)')
     >'       Refl.   Possible reflection conditions'
      write (ludisp,'(a)')
     >'        ---    ------------------------------'
      write (ludisp,'(a)')
     >'        HK0 :  H=2N    K=2N    H+K=2N  H+K=4N'
      write (ludisp,'(a)')
     >'        H0L :  H=2N    L=2N    H+L=2N  H+L=4N'
      write (ludisp,'(a)')
     >'        0KL :  K=2N    L=2N    K+L=2N  K+L=4N'
      write (ludisp,'(a)')
     >'        HHL :  L=2N    L=3N    2H+L=4N'
      write (ludisp,'(a)')
     >'        H00 :  H=2N    H=4N'
      write (ludisp,'(a)')
     >'        0K0 :  K=2N    K=4N'
      write (ludisp,'(a)')
     >'        00L :  L=2N    L=3N    L=4N    L=6N'
      write (ludisp,'(a)')
     >'        HH0 :  H=2N'
      write (ludisp,'(a)')
     >'        H-HL:  H+L=3N  L=2N'
      write (ludisp,'(a)')
     >' '
      write (ludisp,'(a,a)')
     >' As an example consider the case of a 2-fold screw-axis',
     >' along b, then' 
      write (ludisp,'(a)')
     >' the reflexion condition should be:     0K0 : K=2N'
      write (ludisp,'(a,a)')
     >' Only one condition can be entered on each line and should',
     >' conform'
      write (ludisp,'(a)')
     >' to the standard given in the International Tables.'
      write (ludisp,'(a)') ' '

      return
      end

c-----------------------------------------------------------------------

      subroutine exthlp(i)
 1    format (' ',a)
 2    format (' ',a,a)
      write(i,1)
     >'ANALYSE ......: Analyse the list of reflexions wrt conditions'
      write(i,1)
     >'DELETE .......: Delete a condition from the list.'
      write(i,1)
     >'EXIT .........: Return to caller.'
      write(i,2)
     >'INFO .........:',
     >' Get some information how to use reflexion conditions.'
      write(i,1)
     >'INPUT ........: Enter one (or more) reflexion condition(s).'
      write(i,1)
     >'HELP .........: Get some help (this info).'
      write(i,1)
     >'LIST .........: List the saved reflexion conditions.'
      return
      end

c-----------------------------------------------------------------------

      subroutine extdel (lukeys,ludisp,numext,exsymb,str)

      integer    exsymb(50,5)
      character  str*80

      if (str .eq. ' ') then
         write (ludisp,'(a,$)') ' Give number of condition to delete: '
         read (lukeys,'(a)') str
      endif
      if (str .eq. ' ') then
         return
      else
         call upline (str)
         call strip (str)
         if (str .eq. 'ALL' .or.
     >       str .eq. 'AL' .or.
     >       str .eq. 'A' ) then
            numext = 0
         else
            call get_int(str,num,istat)
            if (istat .eq. 0) then
               if (num .ge. 1 .and. num .le. numext) then
                  do i = num,numext-1
                     do j = 1,5
                        exsymb(i,j) = exsymb(i+1,j)
                     enddo
                  enddo
                  numext = numext-1
               endif
            endif
         endif
      endif
 
      return
      end

c-----------------------------------------------------------------------

      logical function extinct (h,k,l)
c
c --- this checks if a reflexion is allowed, according to lattice
c     centering and other entered symmetry restrictions.
c
      include 'puder.cmn'

      integer h,k,l
      logical lattok,symmok,even
c
c --- check lattice extinctions.
c
      if (ilatt .eq. 1) then
         lattok = .true.
      elseif (ilatt.eq.2.and.even(h+k).and.even(h+l).and.even(k+l)) then
         lattok = .true.
      elseif (ilatt.eq.3.and.even(h+k+l)) then
         lattok = .true.
      elseif (ilatt.eq.4.and.even(k+l)) then
         lattok = .true.
      elseif (ilatt.eq.5.and.even(h+l)) then
         lattok = .true.
      elseif (ilatt.eq.6.and.even(h+k)) then
         lattok = .true.
c      elseif (ilatt.eq.7.and.mod(-h+k+l,3).eq.0) then  ! R(obv)
c         lattok = .true.
      elseif (ilatt.eq.7.and.mod(h-k+l,3).eq.0) then   ! R(rev)
         lattok = .true.
      else
         lattok = .false.
      endif
c
c --- check other types of extinctions than lattice extintions.
c     extinction codes stored in the array ixs(i,j)
c
      symmok = .true.

      do 1 i = 1,numext
         isum = h*ixs(i,1) + k*ixs(i,2) + l*ixs(i,3)

c ------ HK0 (type = 1)
         if (l.eq.0 .and.
     >       ixs(i,5).eq.1 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ H0L (type = 2)
         elseif (k.eq.0 .and.
     >           ixs(i,5).eq.2 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ 0KL (type = 3)
         elseif (h.eq.0 .and.
     >           ixs(i,5).eq.3 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ HHL (type = 4)
         elseif (k.eq.h .and.
     >           ixs(i,5).eq.4 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ H00 (type = 5)
         elseif (k.eq.0 .and. l.eq.0 .and.
     >           ixs(i,5).eq.5 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ 0K0 (type = 6)
         elseif (h.eq.0 .and. l.eq.0 .and.
     >           ixs(i,5).eq.6 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ 00L (type = 7)
         elseif (h.eq.0 .and. k.eq.0 .and.
     >           ixs(i,5).eq.7 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ HH0 (type = 8)
         elseif (k.eq.h .and. l.eq.0 .and.
     >           ixs(i,5).eq.8 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

c ------ H-HL (type = 9)
         elseif (k.eq.-h .and. l.ne.0 .and.
     >           ixs(i,5).eq.9 .and. mod(isum,ixs(i,4)).ne.0)then
            symmok = .false.
            goto 2

         endif
 1    continue
c
c --- set return value of this function.
c
 2    continue
      if (lattok .and. symmok) then
         extinct = .false.
      else
         extinct = .true.
      endif

      return
      end
    
c-----------------------------------------------------------------------

      subroutine get_all_weights
c
c --- set the weights to be used in the refinement.
c
      include 'puder.cmn'

      if (iwgtyp .eq. 1) then
c
c ------ Unit weights.
c
         do 1 i = 1,nobs
            weight(i) = 1.0
    1    continue

      elseif (iwgtyp .eq. 2) then
c
c ------ Hess weights,
c
         do 2 i = 1,nobs
            weight(i) = 0.25/(qobs(i)*(1.0-qobs(i)))
    2    continue
c
c ------ If iwgtyp = 3 ie if individual weights are chosen, don't
c        change anything...
c
      elseif (iwgtyp .eq. 4) then
c
c ------ proportional weights,
c
         do 4 i = 1,nobs
            if (nokay(i) .ge. 1) then            
               weight(i) = 1.0/real(nokay(i))
            endif
    4    continue

      elseif (iwgtyp .eq. 5) then
c
c ------ weights = 1/sine(theta)**2.0
c
         do i = 1,nobs
            if (nokay(i) .ge. 1) then            
               weight(i) = 1.0/real(qobs(i))
            endif
         enddo

      elseif (iwgtyp .eq. 6) then
c
c ------ weights = 1/sine(theta)**3.0
c
         do i = 1,nobs
            if (nokay(i) .ge. 1) then            
               weight(i) = 1.0/(real(qobs(i)))**1.5
            endif
         enddo
      endif
c
c --- maybe also a contribution from the esd of the spacing data.
c     First compute the maximum value of 1/sqr(esdobs) in order to be
c     be able to compute a scalefactor for normalizing to 1.
c
      if (use_esdobs_in_weights) then
         do i = 1,nobs
            weight_from_esd(i) = 1.0/sqr(esdobs(i))
         enddo
         wm = weight_from_esd(1)
         do i = 2,nobs
            wm = max(wm,weight_from_esd(i))
         enddo
         scalefactor = 1.0/wm
         do i = 1,nobs
            weight_from_esd(i) = scalefactor*weight_from_esd(i)
         enddo
         do i = 1,nobs
            weight(i) = weight(i)*weight_from_esd(i)
         enddo
      endif 

      return
      end

c-----------------------------------------------------------------------

      subroutine reduce
c
c --- This routine computes the reduced cell using the delaunay 
c     algoritm.
c
      include 'puder.cmn'


      return
      end


c-----------------------------------------------------------------------

      subroutine refine
c
c --- This routine refines a cell, differently depending on the crystal
c     system in use.
c
      include 'puder.cmn'

      call get_all_weights
      call get_all_qcalc
c
c --- choose which crystal system to use when refining ...
c
      if (isyst .eq. 1) then
         call refcub
      elseif (isyst .eq. 2) then
         call reftrig
      elseif (isyst .eq. 3) then
         call reftet
      elseif (isyst .eq. 4) then
         call refhex
      elseif (isyst .eq. 5) then
         call refort
      elseif (isyst .eq. 6) then
         call refmon
      elseif (isyst .eq. 7) then
         call reftric
      else
         write (ludisp,'(a)')
     >   ' Unknown crystal system, no refinement done ...'
      endif
c
c --- compute g(ij) from a(ij)
c
      call getgij

      return
      end

c-----------------------------------------------------------------------

      subroutine get_all_qcalc
c
c --- this routine computes the calculated values of all sine-square
c     theta values etc...
c
      include 'puder.cmn'

      do 1 i = 1,nobs
         if (nokay(i) .ge. 1) then
            qcalc(i) = get_qcalc(ih(i),ik(i),il(i),
     >                   g11,g22,g33,g12,g13,g23)
         else
            qcalc(i) = 0.0
         endif
 1    continue

      return
      end

c-----------------------------------------------------------------------

      subroutine refcub
c
c --- this routine refines a cubic cell.
c
      include 'puder.cmn'

      shkl   = 0.0
      sqhkl = 0.0

      sum = 0.0
      ncalc = 0
      do 1 i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1
            ih2k2l2 = ih(i)*ih(i)+ik(i)*ik(i)+il(i)*il(i)
            shkl = shkl + 
     >             weight(i)*real(ih2k2l2**2)
            sqhkl = sqhkl + 
     >               weight(i)*real(ih2k2l2)*qobs(i)
         endif
    1 continue

      if (shkl .le. 0.5) then
         write (ludisp,'(a)') ' Sorry, no line indexed.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 1
      corr_mat(1,1) = 1.0
c
c --- linear parameters.
c
      g11 = sqhkl/shkl
      if (ncalc .gt. 1) then
         sigg11 = sqrt(sum/(real(ncalc-1)*shkl))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11
      g22 = g11
      sigg22 = sigg11
      g33 = g11
      sigg33 = sigg11
      g12 = 0.0
      sigg12 = 0.0
      g13 = 0.0
      sigg13 = 0.0
      g23 = 0.0
      sigg23 = 0.0

      ast = sqrt(g11)
      sigast = 0.5*rsig11*ast
      bst = ast
      sigbst = sigast
      cst = ast
      sigcst = sigast
      alst = 90.0
      sigalst = 0.0
      best = 90.0
      sigbest = 0.0
      gast = 90.0
      siggast = 0.0

      a = 1.0/ast
      siga = 0.5*rsig11*a
      b = a
      sigb = siga
      c = a
      sigc = siga
      al = 90.0
      sigal = 0.0
      be = 90.0
      sigbe = 0.0
      ga = 90.0
      sigga = 0.0

      return
      end

c-----------------------------------------------------------------------

      subroutine reftrig
c
c --- this routine refines a trigonal cell.
c
      include 'puder.cmn'

      s11     = 0.0
      s12     = 0.0
      s22     = 0.0
      sqh2k2 = 0.0
      sql2   = 0.0

      sum = 0.0
      ncalc = 0
      do i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1
            s11    = s11 + weight(i)*(ih(i)**2+ik(i)**2)**2
            s12    = s12 + weight(i)*(ih(i)**2+ik(i)**2)*il(i)**2
            s22    = s22 + weight(i)*il(i)**4
            sqh2k2 = sqh2k2 + weight(i)*(ih(i)**2+ik(i)**2)*qobs(i)
            sql2   = sql2   + weight(i)*il(i)**2*qobs(i)
         endif
      enddo

      amat(1,1) = s11
      amat(1,2) = s12
      amat(2,1) = s12
      amat(2,2) = s22
      bvec(1) = sqh2k2
      bvec(2) = sql2

      call gaussj(amat,2,bvec,ising)
      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 2
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- linear parameters.
c
      g11 = bvec(1)
      if (ncalc .gt. 2) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-2))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11
      g22 = g11
      sigg22 = sigg11
      g33 = bvec(2)
      if (ncalc .gt. 2) then
         sigg33 = sqrt(amat(2,2)*sum/real(ncalc-2))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33
      g12 = 0.0
      sigg12 = 0.0
      g13 = 0.0
      sigg13 = 0.0
      g23 = 0.0
      sigg23 = 0.0
c
c --- reciprocal cell.
c
      ast = sqrt(g11)
      sigast = 0.5*rsig11*ast
      bst = ast
      sigbst = sigast
      cst = sqrt(g33)
      sigcst = 0.5*rsig33*cst

      alst = 90.0
      best = 90.0
      gast = 90.0
      sigalst = 0.0
      sigbest = 0.0
      siggast = 0.0
c
c --- real cell.
c
      a = 1.0/ast
      siga = 0.5*rsig11*a
      b = a
      sigb = siga
      c = 1.0/cst
      sigc = 0.5*rsig33*c
      al = 90.0
      be = 90.0
      ga = 90.0
      sigal = 0.0
      sigbe = 0.0
      sigga = 0.0

      return
      end

c-----------------------------------------------------------------------

      subroutine reftet
c
c --- this routine refines a tetragonal cell.
c
      include 'puder.cmn'

      s11     = 0.0
      s12     = 0.0
      s22     = 0.0
      sqh2k2 = 0.0
      sql2   = 0.0

      sum = 0.0
      ncalc = 0
      do i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1
            s11    = s11 + weight(i)*(ih(i)**2+ik(i)**2)**2
            s12    = s12 + weight(i)*(ih(i)**2+ik(i)**2)*il(i)**2
            s22    = s22 + weight(i)*il(i)**4
            sqh2k2 = sqh2k2 + weight(i)*(ih(i)**2+ik(i)**2)*qobs(i)
            sql2   = sql2   + weight(i)*il(i)**2*qobs(i)
         endif
      enddo

      amat(1,1) = s11
      amat(1,2) = s12
      amat(2,1) = s12
      amat(2,2) = s22
      bvec(1) = sqh2k2
      bvec(2) = sql2

      call gaussj(amat,2,bvec,ising)
      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 2
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- linear parameters.
c
      g11 = bvec(1)
      if (ncalc .gt. 2) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-2))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11
      g22 = g11
      sigg22 = sigg11
      g33 = bvec(2)
      if (ncalc .gt. 2) then
         sigg33 = sqrt(amat(2,2)*sum/real(ncalc-2))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33
      g12 = 0.0
      sigg12 = 0.0
      g13 = 0.0
      sigg13 = 0.0
      g23 = 0.0
      sigg23 = 0.0
c
c --- reciprocal cell.
c
      ast = sqrt(g11)
      sigast = 0.5*rsig11*ast
      bst = ast
      sigbst = sigast
      cst = sqrt(g33)
      sigcst = 0.5*rsig33*cst

      alst = 90.0
      best = 90.0
      gast = 90.0
      sigalst = 0.0
      sigbest = 0.0
      siggast = 0.0
c
c --- real cell.
c
      a = 1.0/ast
      siga = 0.5*rsig11*a
      b = a
      sigb = siga
      c = 1.0/cst
      sigc = 0.5*rsig33*c
      al = 90.0
      be = 90.0
      ga = 90.0
      sigal = 0.0
      sigbe = 0.0
      sigga = 0.0

      return
      end

c-----------------------------------------------------------------------

      subroutine refhex
c
c --- this routine refines a hexagonal cell.
c
      include 'puder.cmn'

      s11     = 0.0
      s12     = 0.0
      s22     = 0.0
      sqh2hkk2 = 0.0
      sql2   = 0.0

      sum = 0.0
      ncalc = 0
      do 1 i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum      = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc    = ncalc + 1
            ih2hkk2  = ih(i)*ih(i) + ih(i)*ik(i) + ik(i)*ik(i)
            s11      = s11 + weight(i)*ih2hkk2*ih2hkk2
            s12      = s12 + weight(i)*ih2hkk2*il(i)**2
            s22      = s22 + weight(i)*il(i)**4
            sqh2hkk2 = sqh2hkk2 + weight(i)*ih2hkk2*qobs(i)
            sql2     = sql2   + weight(i)*il(i)**2*qobs(i)
         endif
    1 continue

      amat(1,1) = s11
      amat(1,2) = s12
      amat(2,1) = s12
      amat(2,2) = s22
      bvec(1)   = sqh2hkk2
      bvec(2)   = sql2

      call gaussj(amat,2,bvec,ising)
      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 2
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- linear parameters.
c
      g11 = bvec(1)
      if (ncalc .gt. 2) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-2))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11
      g22 = g11
      sigg22 = sigg11
      g33 = bvec(2)
      if (ncalc .gt. 2) then
         sigg33 = sqrt(amat(2,2)*sum/real(ncalc-2))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33
      g12 = g11
      sigg12 = sigg11
      g13 = 0.0
      sigg13 = 0.0
      g23 = 0.0
      sigg23 = 0.0
c
c --- reciprocal cell.
c
      ast = sqrt(g11)
      sigast = 0.5*rsig11*ast
      bst = ast
      sigbst = sigast
      cst = sqrt(g33)
      sigcst = 0.5*rsig33*cst

      alst = 90.0
      best = 90.0
      gast = 60.0
      sigalst = 0.0
      sigbest = 0.0
      siggast = 0.0
c
c --- real cell
c
      a = 1.154700538/ast
      siga = 0.5*rsig11*a
      b = a
      sigb = siga
      c = 1.0/cst
      sigc = 0.5*rsig33*c

      al = 90.0
      be = 90.0
      ga = 120.0
      sigal = 0.0
      sigbe = 0.0
      sigga = 0.0

      return
      end

c-----------------------------------------------------------------------

      subroutine refort
c
c --- this routine refines an ortorhomic cell.
c
      include 'puder.cmn'

      sh4   = 0.0
      sh2k2 = 0.0
      sh2l2 = 0.0
      sk4   = 0.0
      sk2l2 = 0.0
      sl4   = 0.0
      sqh2 = 0.0
      sqk2 = 0.0
      sql2 = 0.0

      sum = 0.0
      ncalc = 0
      do 1 i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1
            rh2 = real(ih(i)*ih(i))
            rk2 = real(ik(i)*ik(i))
            rl2 = real(il(i)*il(i))
            sh4   = sh4   + weight(i)*rh2*rh2
            sh2k2 = sh2k2 + weight(i)*rh2*rk2
            sh2l2 = sh2l2 + weight(i)*rh2*rl2
            sk4   = sk4   + weight(i)*rk2*rk2
            sk2l2 = sk2l2 + weight(i)*rk2*rl2
            sl4   = sl4   + weight(i)*rl2*rl2
            sqh2  = sqh2 + weight(i)*qobs(i)*rh2
            sqk2  = sqk2 + weight(i)*qobs(i)*rk2
            sql2  = sql2 + weight(i)*qobs(i)*rl2
         endif
    1 continue

      amat(1,1) = sh4
      amat(1,2) = sh2k2
      amat(1,3) = sh2l2
      amat(2,1) = sh2k2
      amat(2,2) = sk4
      amat(2,3) = sk2l2
      amat(3,1) = sh2l2
      amat(3,2) = sk2l2
      amat(3,3) = sl4

      bvec(1) = sqh2
      bvec(2) = sqk2
      bvec(3) = sql2

      call gaussj(amat,3,bvec,ising)

      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 3
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- linear parameters.
c
      g11 = bvec(1)
      if (ncalc .gt. 3) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-3))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11
      g22 = bvec(2)
      if (ncalc .gt. 3) then
         sigg22 = sqrt(amat(2,2)*sum/real(ncalc-3))
      else
         sigg22 = 0.0
      endif
      rsig22 = sigg22/g22
      g33 = bvec(3)
      if (ncalc .gt. 3) then
         sigg33 = sqrt(amat(3,3)*sum/real(ncalc-3))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33
      g12 = 0.0
      sigg12 = 0.0
      g13 = 0.0
      sigg13 = 0.0
      g23 = 0.0
      sigg23 = 0.0
c
c --- reciprocal cell.
c
      ast = sqrt(g11)
      sigast = 0.5*rsig11*ast
      bst = sqrt(g22)
      sigbst = 0.5*rsig22*bst
      cst = sqrt(g33)
      sigcst = 0.5*rsig33*cst
      alst = 90.0
      sigalst = 0.0
      best = 90.0
      sigbest = 0.0
      gast = 90.0
      siggast = 0.0
c
c --- real cell.
c
      a = 1.0/ast
      siga = 0.5*rsig11*a
      b = 1.0/bst
      sigb = 0.5*rsig22*b
      c = 1.0/cst
      sigc = 0.5*rsig33*c
      al = 90.0
      sigal = 0.0
      be = 90.0
      sigbe = 0.0
      ga = 90.0
      sigga = 0.0

      return
      end

c-----------------------------------------------------------------------

      subroutine refmon
c
c --- this routine refines a monoclinic cell.
c
      include 'puder.cmn'

      sh4   = 0.0
      sh2k2 = 0.0
      sh2l2 = 0.0
      sh3l  = 0.0
      sk4   = 0.0
      sk2l2 = 0.0
      shk2l = 0.0
      sl4   = 0.0
      shl3  = 0.0
      sqh2 = 0.0
      sqk2 = 0.0
      sql2 = 0.0
      sqhl = 0.0

      sum = 0.0
      ncalc = 0
      do 1 i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1

            rh2   = real(ih(i)*ih(i))
            rk2   = real(ik(i)*ik(i))
            rl2   = real(il(i)*il(i))
            rhl   = real(ih(i)*il(i))

            sh4   = sh4   + weight(i)*rh2*rh2
            sh2k2 = sh2k2 + weight(i)*rh2*rk2
            sh2l2 = sh2l2 + weight(i)*rh2*rl2
            sh3l  = sh3l  + weight(i)*rh2*rhl

            sk4   = sk4   + weight(i)*rk2*rk2
            sk2l2 = sk2l2 + weight(i)*rk2*rl2
            shk2l = shk2l + weight(i)*rhl*rk2

            sl4   = sl4   + weight(i)*rl2*rl2
            shl3  = shl3  + weight(i)*rhl*rl2

            sqh2  = sqh2 + weight(i)*qobs(i)*rh2
            sqk2  = sqk2 + weight(i)*qobs(i)*rk2
            sql2  = sql2 + weight(i)*qobs(i)*rl2
            sqhl  = sqhl + weight(i)*qobs(i)*rhl
         endif
    1 continue

      amat(1,1) = sh4
      amat(1,2) = sh2k2
      amat(1,3) = sh2l2
      amat(1,4) = sh3l

      amat(2,1) = sh2k2
      amat(2,2) = sk4
      amat(2,3) = sk2l2
      amat(2,4) = shk2l

      amat(3,1) = sh2l2
      amat(3,2) = sk2l2
      amat(3,3) = sl4
      amat(3,4) = shl3

      amat(4,1) = sh3l
      amat(4,2) = shk2l
      amat(4,3) = shl3
      amat(4,4) = sh2l2

      bvec(1) = sqh2
      bvec(2) = sqk2
      bvec(3) = sql2
      bvec(4) = sqhl

      call gaussj(amat,4,bvec,ising)

      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 4
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- linear parameters
c
      g11 = bvec(1)
      if (ncalc .gt. 4) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-4))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11

      g22 = bvec(2)
      if (ncalc .gt. 4) then
         sigg22 = sqrt(amat(2,2)*sum/real(ncalc-4))
      else
         sigg22 = 0.0
      endif
      rsig22 = sigg22/g22

      g33 = bvec(3)
      if (ncalc .gt. 4) then
         sigg33 = sqrt(amat(3,3)*sum/real(ncalc-4))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33

      g12 = 0.0
      sigg12 = 0.0

      g13 = bvec(4)
      if (ncalc .gt. 4) then
         sigg13 = sqrt(amat(4,4)*sum/real(ncalc-4))
      else
         sigg13 = 0.0
      endif
      rsig13 = sigg13/g13

      g23 = 0.0
      sigg23 = 0.0
c
c --- reciprocal cell.
c
      ast = sqrt(g11)
      sigast = 0.5*ast*rsig11

      bst = sqrt(g22)
      sigbst = 0.5*bst*rsig22

      cst = sqrt(g33)
      sigcst = 0.5*cst*rsig33

      alst = 90.0
      sigalst = 0.0

c compute sigcosbetastar
      cbs = g13/(2.0*ast*cst)
      rsigcbs = sqrt(sqr(rsig13) + sqr(rsig11) + sqr(rsig33))
cold      rsigcbs = sqrt(rsig13*rsig13 +
cold     >             sqrt(2.0)*(rsig11*rsig11+rsig33*rsig33) )
      sigcbs = rsigcbs*cbs
c compute betastar, remember to convert to degree measure.
      best = todeg(acos(cbs))
      sigbest = todeg(0.5*abs(acos(cbs+sigcbs)-acos(cbs-sigcbs)))

      gast = 90.0
      siggast = 0.0

      a = 1.0/(ast*sin(torad(be)))
      siga = 0.5*a*rsig11
      b = 1.0/bst
      sigb = 0.5*b*rsig22
      c = 1.0/(cst*sin(torad(be)))
      sigc = 0.5*c*rsig33
      al = 90.0
      be = 180.0-best
      sigbe = sigbest
      ga = 90.0

      return
      end

c-----------------------------------------------------------------------

      subroutine reftric
c
c --- this routine refines a triclinic cell.
c
      include 'puder.cmn'

      sum = 0.0
      ncalc = 0

      call matzer (amat,6)
      call veczer (bvec,6)

      do 2 i = 1,nobs
         if ( (nokay(i) .gt. 0 .and. nokay(i) .le. linuse) .or.
     >       ilock(i) .eq. 1 ) then
            sum = sum + weight(i)*sqr(qobs(i)-qcalc(i))
            ncalc = ncalc + 1

            rh2 = real(ih(i)*ih(i))
            rk2 = real(ik(i)*ik(i))
            rl2 = real(il(i)*il(i))
            rhk = real(ih(i)*ik(i))
            rhl = real(ih(i)*il(i))
            rkl = real(ik(i)*il(i))

            amat(1,1) = amat(1,1) + weight(i)*rh2*rh2
            amat(1,2) = amat(1,2) + weight(i)*rh2*rk2
            amat(1,3) = amat(1,3) + weight(i)*rh2*rl2
            amat(1,4) = amat(1,4) + weight(i)*rh2*rhk
            amat(1,5) = amat(1,5) + weight(i)*rh2*rhl
            amat(1,6) = amat(1,6) + weight(i)*rh2*rkl

            amat(2,2) = amat(2,2) + weight(i)*rk2*rk2
            amat(2,3) = amat(2,3) + weight(i)*rk2*rl2
            amat(2,4) = amat(2,4) + weight(i)*rk2*rhk
            amat(2,5) = amat(2,5) + weight(i)*rk2*rhl
            amat(2,6) = amat(2,6) + weight(i)*rk2*rkl

            amat(3,3) = amat(3,3) + weight(i)*rl2*rl2
            amat(3,4) = amat(3,4) + weight(i)*rl2*rhk
            amat(3,5) = amat(3,5) + weight(i)*rl2*rhl
            amat(3,6) = amat(3,6) + weight(i)*rl2*rkl

            amat(4,4) = amat(4,4) + weight(i)*rhk*rhk
            amat(4,5) = amat(4,5) + weight(i)*rhk*rhl
            amat(4,6) = amat(4,6) + weight(i)*rhk*rkl

            amat(5,5) = amat(5,5) + weight(i)*rhl*rhl
            amat(5,6) = amat(5,6) + weight(i)*rhl*rkl

            amat(6,6) = amat(6,6) + weight(i)*rkl*rkl

            bvec(1) = bvec(1) + weight(i)*qobs(i)*rh2
            bvec(2) = bvec(2) + weight(i)*qobs(i)*rk2
            bvec(3) = bvec(3) + weight(i)*qobs(i)*rl2
            bvec(4) = bvec(4) + weight(i)*qobs(i)*rhk
            bvec(5) = bvec(5) + weight(i)*qobs(i)*rhl
            bvec(6) = bvec(6) + weight(i)*qobs(i)*rkl

         endif
    2 continue
c
c --- set the lower half of the coefficient matrix.
c
      do 3 irow = 2,6
         do 4 icol = 1,irow-1
            amat(irow,icol) = amat(icol,irow)
    4    continue
    3 continue

      call gaussj (amat,6,bvec,ising)
      if (ising .eq. 1) then
         write (ludisp,'(a)') ' Sorry, singular least-square matrix.'
         return
      endif
c
c --- Correlation matrix.
c
      n_corr_mat = 6
      do irow = 1,n_corr_mat
         do icol = 1,n_corr_mat
            corr_mat(irow,icol) = amat(irow,icol) /
     >      (sqrt(amat(irow,irow))*sqrt(amat(icol,icol)))
         enddo
      enddo
c
c --- Set the linear parameteres and compute their esd's
c
      g11 = bvec(1)
      if (ncalc .gt. 6) then
         sigg11 = sqrt(amat(1,1)*sum/real(ncalc-6))
      else
         sigg11 = 0.0
      endif
      rsig11 = sigg11/g11

      g22 = bvec(2)
      if (ncalc .gt. 6) then
         sigg22 = sqrt(amat(2,2)*sum/real(ncalc-6))
      else
         sigg22 = 0.0
      endif
      rsig22 = sigg22/g22

      g33 = bvec(3)
      if (ncalc .gt. 6) then
         sigg33 = sqrt(amat(3,3)*sum/real(ncalc-6))
      else
         sigg33 = 0.0
      endif
      rsig33 = sigg33/g33

      g12 = bvec(4)
      if (ncalc .gt. 6) then
         sigg12 = sqrt(amat(4,4)*sum/real(ncalc-6))
      else
         sigg12 = 0.0
      endif
      rsig12 = sigg12/g12

      g13 = bvec(5)
      if (ncalc .gt. 6) then
         sigg13 = sqrt(amat(5,5)*sum/real(ncalc-6))
      else
         sigg13 = 0.0
      endif
      rsig13 = sigg13/g13

      g23 = bvec(6)
      if (ncalc .gt. 6) then
         sigg23 = sqrt(amat(6,6)*sum/real(ncalc-6))
      else
         sigg23 = 0.0
      endif
      rsig23 = sigg23/g23
c
c --- Compute the reciprocal cell-parameters and their esd's
c     from the linear parameters.
c
      ast = sqrt(g11)
      sigast = 0.5*ast*rsig11

      bst = sqrt(g22)
      sigbst = 0.5*bst*rsig22

      cst = sqrt(g33)
      sigcst = 0.5*cst*rsig33

c compute sigcosalfastar
      cas = g23/(2.0*bst*cst)
      rsigcas = sqrt(sqr(rsig23) + sqr(rsig22) + sqr(rsig33))
      sigcas = rsigcas*cas
c compute alfastar, remember to convert to degree measure.
      alst = todeg(acos(cas))
      sigalst = todeg(0.5*abs(acos(cas+sigcas)-acos(cas-sigcas)))

c compute sigcosbetastar
      cbs = g13/(2.0*ast*cst)
      rsigcbs = sqrt(sqr(rsig13) + sqr(rsig11) + sqr(rsig33) )
      sigcbs = rsigcbs*cbs
c compute betastar, remember to convert to degree measure.
      best = todeg(acos(cbs))
      sigbest = todeg(0.5*abs(acos(cbs+sigcbs)-acos(cbs-sigcbs)))

c compute sigcosgammastar
      cgs = g12/(2.0*ast*bst)
      rsigcgs = sqrt(sqr(rsig12) + sqr(rsig11) + sqr(rsig22) )
      sigcgs = rsigcgs*cgs
c compute gammastar, remember to convert to degree measure.
      gast = todeg(acos(cgs))
      siggast = todeg(0.5*abs(acos(cgs+sigcgs)-acos(cgs-sigcgs)))
c
c --- Compute the real cell-parameters and their esd's
c     from the reciprocal parameters and the esd's of the linear gij.
c
      vst = vol (ast,bst,cst,torad(alst),torad(best),torad(gast))
      a = bst*cst*sin(torad(alst)) / vst
      b = ast*cst*sin(torad(best)) / vst
      c = ast*bst*sin(torad(gast)) / vst
      al = acos ( (cos(torad(best))*cos(torad(gast))-cos(torad(alst))) /
     >            (sin(torad(best))*sin(torad(gast))) )
      be = acos ( (cos(torad(alst))*cos(torad(gast))-cos(torad(best))) /
     >            (sin(torad(alst))*sin(torad(gast))) )
      ga = acos ( (cos(torad(alst))*cos(torad(best))-cos(torad(gast))) /
     >            (sin(torad(alst))*sin(torad(best))) )
      al = todeg(al)
      be = todeg(be)
      ga = todeg(ga)

      siga = 0.5*a*rsig11
      sigb = 0.5*b*rsig22
      sigc = 0.5*c*rsig33
      sigal = sigalst
      sigbe = sigbest
      sigga = siggast

      return
      end

c-----------------------------------------------------------------------

      subroutine cycref (str)
c
c --- this routine refines an cell several cycles and writes the 
c     indexed pattern afterwards.
c
      include 'puder.cmn'

      character str*80, str80*80
      logical   okqobs
c
c --- first check if descent observed values are present.
c
      if (.not.okqobs(ludisp,qobs,nobs,wave)) then
         return
      endif
c
c --- go on with decoding the parameter string.
c
      if (str .eq. ' ') then
         write (ludisp,'(a,$)') ' Give number of cycles:'
         read  (lukeys,'(a)') str
      endif
      if (str .eq. ' ') then
         return
      else
         call strip(str)
         call upline(str)
         if (str .eq. 'ONE') then
            ncyc = 1
         elseif (str .eq. 'TWO') then
            ncyc = 2
         elseif (str .eq. 'THREE') then
            ncyc = 3
         elseif (str .eq. 'FOUR') then
            ncyc = 4
         elseif (str .eq. 'FIVE') then
            ncyc = 5
         elseif (str .eq. 'SIX') then
            ncyc = 6
         elseif (str .eq. 'SEVEN') then
            ncyc = 7
         elseif (str .eq. 'EIGHT') then
            ncyc = 8
         elseif (str .eq. 'NINE') then
            ncyc = 9
         elseif (str .eq. 'TEN') then
            ncyc = 10
         else
            call get_int(str,ncyc,istat)
            if (istat .ne. 0) then
               return
            endif
         endif
      endif

      if (ncyc .le. 0) then
         return
      endif

      write (ludisp,'(a)') ' '
      write (ludisp,'(a)') ' Cycle results.'
      write (ludisp,'(a)') ' '
      if (ilogfl .eq. 1) then
         write (lulog,'(a)') ' '
         write (lulog,'(a)') ' Cycle results.'
         write (lulog,'(a)') ' '
      endif

      write (ludisp,'(i4,a,3f10.5,3f10.4)') 0,':',a,b,c,al,be,ga
      if (ilogfl .eq. 1) then
         write (lulog,'(a)') ' '
         write (lulog,'(i4,a,3f10.5,3f10.4)') 0,':',a,b,c,al,be,ga
      endif
      str80 = ' '
      do icyc = 1,ncyc
         call indlin (str80)
         call refine
         write (ludisp,'(i4,a,3f10.5,3f10.4)') icyc,':',a,b,c,al,be,ga
         if (ilogfl .eq. 1) then
            write (lulog,'(i4,a,3f10.5,3f10.4)') icyc,':',a,b,c,al,be,ga
         endif
      enddo

      write (ludisp,'(a)') ' '
      if (ilogfl .eq. 1) then
         write (lulog,'(a)') ' '
      endif

      call sqwrit (wr_default)
      if (ilogfl .eq. 1) then
         write (lulog,'(a)') ' '
      endif
      str80 = '20'
      call merits (str80)
      return
      end

c-----------------------------------------------------------------------

      subroutine matzer (a,n)
      dimension a(6,6)
      do 1 i = 1,n
         do 2 j = 1,n
            a(i,j) = 0.0
 2       continue
 1    continue
      return
      end

c-----------------------------------------------------------------------

      subroutine veczer (a,n)
      dimension a(6)
      do 1 i = 1,n
         a(i) = 0.0
 1    continue
      return
      end

c-----------------------------------------------------------------------

      function sqr(x)
      sqr = x*x
      return
      end

c-----------------------------------------------------------------------

      function det2 (d11,d12,d21,d22)
      det2 = d11*d22-d12*d21
      return
      end

c-----------------------------------------------------------------------

      integer function idet2 (i11,i12,i21,i22)
      idet2 = i11*i22-i12*i21
      return
      end

c-----------------------------------------------------------------------

      integer function idet3 (i11,i12,i13, i21,i22,i23, i31,i32,i33)
      idet3 = i11*idet2(i22,i23,i32,i33)+
     >        i12*idet2(i23,i21,i33,i31)+
     >        i13*idet2(i21,i22,i31,i32)
      return
      end

c-----------------------------------------------------------------------

      integer function idet4 (i11,i12,i13,i14, 
     >                        i21,i22,i23,i24, 
     >                        i31,i32,i33,i34, 
     >                        i41,i42,i43,i44)
      idet4 = i11*idet3(i22,i23,i24, i32,i33,i34, i42,i43,i44) +
     >        i12*idet3(i23,i24,i21, i33,i34,i31, i43,i44,i41) +
     >        i13*idet3(i24,i21,i22, i34,i31,i32, i44,i41,i42) +
     >        i14*idet3(i21,i22,i23, i31,i32,i33, i41,i42,i43)
      return
      end

c-----------------------------------------------------------------------

      integer function idet5 (i11,i12,i13,i14,i15, 
     >                        i21,i22,i23,i24,i25,
     >                        i31,i32,i33,i34,i35, 
     >                        i41,i42,i43,i44,i45,
     >                        i51,i52,i53,i54,i55)
      idet5 = i11*idet4(i22,i23,i24,i25, i32,i33,i34,i35, 
     >                  i42,i43,i44,i45, i52,i53,i54,i55) +
     >        i12*idet4(i23,i24,i25,i21, i33,i34,i35,i31, 
     >                  i43,i44,i45,i41, i53,i54,i55,i51) +
     >        i13*idet4(i24,i25,i21,i22, i34,i35,i31,i32, 
     >                  i44,i45,i41,i42, i54,i55,i51,i52) +
     >        i14*idet4(i25,i21,i22,i23, i35,i31,i32,i33, 
     >                  i45,i41,i42,i43, i55,i51,i52,i53) +
     >        i15*idet4(i21,i22,i23,i24, i31,i32,i33,i34,
     >                  i41,i42,i43,i44, i51,i52,i53,i54)
      return
      end

c-----------------------------------------------------------------------

      integer function idet6 (i11,i12,i13,i14,i15,i16, 
     >                        i21,i22,i23,i24,i25,i26,
     >                        i31,i32,i33,i34,i35,i36,
     >                        i41,i42,i43,i44,i45,i46,
     >                        i51,i52,i53,i54,i55,i56)
      idet6 = i11*idet5(i22,i23,i24,i25,i26, i32,i33,i34,i35,i36, 
     >                  i42,i43,i44,i45,i26, i52,i53,i54,i55,i56,
     >                  i62,i63,i64,i65,i66) +
     >        i12*idet5(i23,i24,i25,i26,i21, i33,i34,i35,i36,i31, 
     >                  i43,i44,i45,i46,i41, i53,i54,i55,i56,i51, 
     >                  i63,i64,i65,i66,i61) +
     >        i13*idet5(i24,i25,i26,i21,i22, i34,i35,i36,i31,i32, 
     >                  i44,i45,i46,i41,i42, i54,i55,i56,i51,i52,
     >                  i64,i65,i66,i61,i62) +
     >        i14*idet5(i25,i26,i21,i22,i23, i35,i36,i31,i32,i33, 
     >                  i45,i46,i41,i42,i43, i55,i56,i51,i52,i53,
     >                  i65,i66,i61,i62,i63) +
     >        i15*idet5(i26,i21,i22,i23,i24, i36,i31,i32,i33,i34,
     >                  i46,i41,i42,i43,i44, i56,i51,i52,i53,i54,
     >                  i66,i61,i62,i63,i64) +
     >        i16*idet5(i21,i22,i23,i24,i25, i31,i32,i33,i34,i35,
     >                  i41,i42,i43,i44,i45, i51,i52,i53,i54,i55,
     >                  i61,i62,i63,i64,i65)
      return
      end

c-----------------------------------------------------------------------

      function torad(x)
      torad = x*0.01745329252
      return
      end

c-----------------------------------------------------------------------

      function todeg(x)
      todeg = x*57.29577951
      return
      end

c-----------------------------------------------------------------------

      subroutine hsort2 (v,ix,antal)
c
c --- This routine sorts the numbers in the array V in increasing order
c     with the help of heap sort ...
c     IX is a help array with indexes that map the old V-array
c     onto the permuted V-array.
c
      integer   antal
      dimension v(1),ix(1)
c
c --- Create the heap.
c
      call crhep2 (v,ix,antal)
c
c --- Sort the heap.
c
      do 100 i = antal,2,-1
         call swapr (v(i),v(1))
         call swapi (ix(i),ix(1))
         irot = 1
         call adhep2 (v,ix,irot,i-1)
  100 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine crhep2 (v,ix,antal)
c
c --- This routine creates the heap ...
c
      integer   antal
      dimension v(1),ix(1)
c
      do 100 i = antal/2,1,-1
         irot = i
         call adhep2 (v,ix,irot,antal)
  100 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine adhep2 (v,ix,rot,n)
c
c --- This routine adjusts the heap ...
c
      integer   rot,n
      dimension v(1),ix(1)

 4711 continue
      j = 2*rot
      if (j .le. n) then
         if (j .lt. n) then
            if (v(j) .lt. v(j+1)) then
               j = j+1
            endif
         endif
         if (v(rot) .lt. v(j)) then
            call swapr (v(j),v(rot))
            call swapi (ix(j),ix(rot))
            rot = j
            goto 4711
         endif
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine swapr (ra,rb)
c
c --- This routine swaps the two real variables RA and RB.
c
      temp = ra
      ra   = rb
      rb   = temp
c
      return
      end

c-----------------------------------------------------------------------

      subroutine swapi (ia,ib)
c
c --- This routine swaps the two integer variables IA and IB.
c
      itemp = ia
      ia    = ib
      ib    = itemp
c
      return
      end

c-----------------------------------------------------------------------

      subroutine ivecrea (iarr,ixarr,itemp,n)
c
c --- This routine rearranges the vector IARR according to the indexes
c     in the vector IXARR.
c     ITEMP is a temporary work array.
c
      dimension iarr(1),ixarr(1),itemp(1)
c
c --- Set the elements of the ITEMP array.
c
      do 100 i = 1,n
         itemp(i) = iarr(ixarr(i))
  100 continue
c
c --- Copy ITEMP array to IARR.
c
      do 200 i = 1,n
         iarr(i) = itemp(i)
  200 continue

      return
      end

c-----------------------------------------------------------------------

      subroutine index_very_interactively
c
c --- this routine is the first version of a very interactive indexing
c     routine...
c
      include 'puder.cmn'
c
c --- declarations for local variables.
c
      integer   used_lines, base_table(6)
      integer   hkltyp, baserow
      character ch*1, cmd*80, cpar*80,str80*80
      logical   baseline(20)
c
c --- some defintions...
c
      nbase = 0
      used_lines = min(nobs,20)
      do i = 1,20
         baseline(i) = .false.
      enddo
c
c --- clear screen, write header etc.
c
  998 continue
      write(ludisp,'(a)') ' '
      write(ludisp,'(a,a,a)') 
     >'  B     N    H    K    L    Qobs     Qcalc     Qdiff  ',
     >'   2th-obs  2th-calc  2th-diff','    #'
      write(ludisp,'(a,a,a)') 
     >' --- ---- ---- ---- ---- --------- --------- ---------',
     >' --------- --------- ---------',' ----'
c      12341234512345123451234512345678901234567890123456789012345
c      ++++!++++1++++!++++2++++!++++3++++!++++4++++!++++5++++!++++6
c
c --- write the spacing data
c
      nb = 0
      do i = 1,used_lines
c
c ------ "base" line?
c
         if (baseline(i)) then
            nb = nb+1
            ttobs = 2*todeg(asin(wave*0.5*sqrt(qobs(i))))
            if (nokay(i) .gt. 0) then
               ttcalc = 2*todeg(asin(wave*0.5*sqrt(qcalc(i))))
               ttdiff = ttobs-ttcalc
               write(ludisp,'(a,i1,a,4i5,3f10.5,3f10.4,i5)') 
     >         ' (',nb,')',i,ih(i),ik(i),il(i),
     >         qobs(i),qcalc(i),qobs(i)-qcalc(i),
     >         ttobs,ttcalc,ttdiff,
     >         nokay(i)
            else 
               write(ludisp,'(a,i1,a,4i5,f10.5,a,f10.4)') 
     >         ' (',nb,')',i,ih(i),ik(i),il(i),
     >         qobs(i),'                    ',ttobs
            endif
c
c ------ or a "normal" line
c
         else
            ttobs = 2*todeg(asin(wave*0.5*sqrt(qobs(i))))
            if (nokay(i) .gt. 0) then
               ttcalc = 2*todeg(asin(wave*0.5*sqrt(qcalc(i))))
               ttdiff = ttobs-ttcalc
               write(ludisp,'(a,4i5,3f10.5,3f10.4,i5)') 
     >         '    ',i,ih(i),ik(i),il(i),
     >         qobs(i),qcalc(i),qobs(i)-qcalc(i),
     >         ttobs,ttcalc,ttdiff,nokay(i)
            else
               write(ludisp,'(a,4i5,f10.5,a,f10.4)') 
     >         '    ',i,ih(i),ik(i),il(i),
     >         qobs(i),'                    ',ttobs
            endif
         endif
      enddo
c
c --- write cell.
c
      call write_cell
     >(ludisp,g11,g22,g33,g12,g13,g23,a,b,c,al,be,ga)
c
c --- get next character typed at keyboard and take an 
c     appropriate action below.
c
      write(ludisp,'(a,$)') ' Interactive indexing (? for help) -> '

      read(lukeys,'(a)') cpar
      if (cpar .ne. ' ') then
         call move_first_element (cpar,cmd)
         if (cpar .ne. ' ') then
            call strip (cpar)
         endif
         call upline(cmd)
         call strip(cmd)
      endif
c
c --- Help information.
c
      if (cmd .eq. '?') then
         call describe_interactive_commands(ludisp)
c
c --- increment baseline-index.
c
      elseif (cmd .eq. '+' .and. nbase .gt. 0) then
         if (nbase .gt. 0) then
            i = base_table(baserow)
            if (hkltyp .eq. 1) then
               ih(i) = ih(i)+1
            elseif (hkltyp .eq. 2) then
               ik(i) = ik(i)+1
            elseif (hkltyp .eq. 3) then
               il(i) = il(i)+1
            endif
            if (ih(i).ne.0 .or. ik(i).ne.0 .or. il(i).ne.0 ) then
               nokay(i) = 1
               qcalc(i) = get_qcalc(ih(i),ik(i),il(i),
     >                              g11,g22,g33,g12,g13,g23)
            else
               nokay(i) = 0
            endif
         else
            call write_base_error_at_23(ludisp)
         endif
c
c --- decrement baseline-index.
c
      elseif (cmd .eq. '-') then
         if (nbase .gt. 0) then
            i = base_table(baserow)
            if (hkltyp .eq. 1) then
               ih(i) = ih(i)-1
            elseif (hkltyp .eq. 2) then
               ik(i) = ik(i)-1
            elseif (hkltyp .eq. 3) then
               il(i) = il(i)-1
            endif
            if (ih(i).ne.0 .or. ik(i).ne.0 .or. il(i).ne.0 ) then
               nokay(i) = 1
               qcalc(i) = get_qcalc(ih(i),ik(i),il(i),
     >                              g11,g22,g33,g12,g13,g23)
            else
               nokay(i) = 0
            endif
         else
            call write_base_error_at_23(ludisp)
         endif
c
c --- Set the base-lines.
c
      elseif (cmd .eq. 'B') then
         if (isyst .eq. 1) then
            nbase = 1
         elseif (isyst .ge. 2 .and. isyst .le. 4) then
            nbase = 2
         elseif (isyst .eq. 5) then
            nbase = 3
         elseif (isyst .eq. 6) then
            nbase = 4
         elseif (isyst .eq. 7) then
            nbase = 6
         else 
            nbase = 0
         endif

         if (nbase .gt. 0) then
            call set_base_lines(
     >      lukeys,ludisp,nbase,baseline,base_table,used_lines)
            baserow = 1
         else
            write(ludisp,'(a,a,$)') 
     >      'You must define the Crystal System before base-line',
     >      ' defintion.'
         endif
c
c --- Solve for cell-parameters.
c
      elseif (cmd .eq. 'C') then
         call solve_cell_parameters
     >   (ludisp,ih,ik,il,qobs,base_table,isyst,
     >   g11,g22,g33,g12,g13,g23, iok)
         if (iok .eq. 1) then
            call gij_to_reciprocal
     >      (g11,g22,g33,g12,g13,g23, ast,bst,cst,alst,best,gast)
            call reciprocal_to_real
     >      (ast,bst,cst,alst,best,gast, a,b,c,al,be,ga)
         endif
c
c --- set the acceptance window
c
      elseif (cmd .eq. 'D') then
         write(ludisp,'(a,$)')
     >   'Enter max 2theta acceptance window:'
         read (lukeys,*,iostat=istat) rtemp
         if (istat .eq. 0) then
            delta_twotheta = rtemp
         endif
c
c --- Exit from this routine, back to puder top-level.
c
      elseif (cmd .eq. 'E') then
         return
c
c --- Change some baseline H-index.  
c
      elseif (cmd .eq. 'H') then
         if (nbase .gt. 0) then
            if (cpar .eq. ' ') then
               write(ludisp,'(a,$)') 
     >         ' Enter base line number for changing H : '
               read(lukeys,'(a)') cpar
            endif
            call get_int(cpar,nbtemp,istat)
            if (istat .eq. 0 .and. 
     >          nbtemp .gt. 0 .and. 
     >          nbtemp .le. nbase) then
               hkltyp = 1
               baserow = nbtemp
            endif
         else
            call write_base_error_at_23(ludisp)
         endif
c
c --- Index the lines.
c
      elseif (cmd .eq. 'I') then
          str80 = ' '
          if (iverb .eq. 1) then
             iverb = 0
             call indlin (str80)
             iverb = 1
          else
             call indlin (str80)
          endif
c
c --- Change some baseline K-index.  
c
      elseif (cmd .eq. 'K') then
         if (nbase .gt. 0) then
            if (cpar .eq. ' ') then
               write(ludisp,'(a,$)') 
     >         ' Enter base line number for changing K : '
               read(lukeys,'(a)') cpar
            endif
            call get_int(cpar,nbtemp,istat)
            if (istat .eq. 0 .and. 
     >          nbtemp .gt. 0 .and. 
     >          nbtemp .le. nbase) then
               hkltyp = 2
               baserow = nbtemp
            endif
         else
            call write_base_error_at_23(ludisp)
         endif
c
c --- Change some baseline L-index.  
c
      elseif (cmd .eq. 'L') then
         if (nbase .gt. 0) then
            if (cpar .eq. ' ') then
               write(ludisp,'(a,$)') 
     >         ' Enter base line number for changing L : '
               read(lukeys,'(a)') cpar
            endif
            call get_int(cpar,nbtemp,istat)
            if (istat .eq. 0 .and. 
     >          nbtemp .gt. 0 .and. 
     >          nbtemp .le. nbase) then
               hkltyp = 3
               baserow = nbtemp
            endif
         else
            call write_base_error_at_23(ludisp)
         endif
c
c --- Refine the unit cell.
c
      elseif (cmd .eq. 'R') then
         call refine
c
c --- Set the crystal system.
c
      elseif (cmd .eq. 'S') then
         write(ludisp,'(a,a,$)')
     >   'Enter Crystal System: 1) Cub, 2) Trig, 3)Tet, 4) Hex,',
     >   ' 5) Ort, 6) Mon or 7) Tri :'
         call getchar(ch)
         if (ch .eq. '1') then
            new_isyst = 1
         elseif (ch .eq. '2') then
            new_isyst = 2
         elseif (ch .eq. '3') then
            new_isyst = 3
         elseif (ch .eq. '4') then
            new_isyst = 4
         elseif (ch .eq. '5') then
            new_isyst = 5
         elseif (ch .eq. '6') then
            new_isyst = 6
         elseif (ch .eq. '7') then
            new_isyst = 7
         endif
         if (new_isyst .ge. 1 .and. new_isyst .le. 7) then
            nbase = 0
            do i = 1,used_lines
               baseline(i) = .false.
            enddo            
            isyst = new_isyst
         endif
c
c --- Set the parameter LINUSE.
c
      elseif (cmd .eq. 'U') then
         write(ludisp,'(a,$)')
     >   'Enter max # of HKL / line for use in refinement procedure:'
         read (lukeys,*,iostat=istat) itemp
         if (istat .eq. 0) then
            linuse = itemp
         endif
c
c --- Clear all indexes.
c
      elseif (cmd .eq. 'Z') then
         do i = 1,nobs
            ih(i) = 0
            ik(i) = 0
            il(i) = 0
            nokay(i) = 0
            ilock(i) = 0
         enddo

      endif
c
c --- loop back for reading another command.
c
      goto 998

      end

c-----------------------------------------------------------------------

      subroutine describe_interactive_commands(ludisp)

      character ch*1

      i = ludisp

    1 format(a)
      write(i,1)
     >' '
      write(i,1)
     >'CMD Short description'
      write(i,1)
     >'(?) Get a list of available commands.'
      write(i,1)
     >'(B) Set the baselines.'
      write(i,1)
     >'(+) Increment H,K or L of baseline.'
      write(i,1)
     >'(-) Decrement H,K or L of baseline.'
      write(i,1)
     >'(C) Solve for cell-parameters (if possible).'
      write(i,1)
     >'(D) Set delta 2theta acceptance window.'
      write(i,1)
     >'(E) Exit from this very routine.'
      write(i,1)
     >'(I) Index the lines (if possible).'
      write(i,1)
     >'(H) Manipulate the H-index for one baseline'
      write(i,1)
     >'(K) Manipulate the K-index for one baseline'
      write(i,1)
     >'(L) Manipulate the L-index for one baseline'
      write(i,1)
     >'(R) Refine the unit cell with the current data.'
      write(i,1)
     >'(S) Set the crystal system to be used.'
      write(i,1)
     >'(U) Use maximum number of different indexing in refinement.'
      write(i,1)
     >'(Z) Zero all indexes for all lines.'

      write(i,'(a,$)') 'Press RETURN to continue...'
      write(i,1)
     >' '
      call getchar(ch)

      return
      end

c-----------------------------------------------------------------------
      subroutine write_gij_parameters

      include 'puder.cmn'

      write(ludisp,'(a,6f10.7)') ' gij: ',g11,g22,g33,g12,g13,g23

      return
      end

c-----------------------------------------------------------------------

      subroutine write_cell
     >(ludisp,g11,g22,g33,g12,g13,g23,a,b,c,al,be,ga)

      write(ludisp,'(a)') ' '
      write(ludisp,'(a,6f10.6)') ' gij: ',g11,g22,g33,g12,g13,g23
      write(ludisp,'(a,6f10.4)') ' Cell:',a,b,c,al,be,ga
      write(ludisp,'(a)') ' '

      return
      end

c-----------------------------------------------------------------------

      subroutine gij_to_reciprocal
     > (g11,g22,g33,g12,g13,g23, ast,bst,cst,alst,best,gast)
c
c --- Compute the reciprocal cell-parameters
c     from the linear parameters.
c
      ast = sqrt(g11)
      bst = sqrt(g22)
      cst = sqrt(g33)

      cas = g23/(2.0*bst*cst)
      alst = todeg(acos(cas))

      cbs = g13/(2.0*ast*cst)
      best = todeg(acos(cbs))

      cgs = g12/(2.0*ast*bst)
      gast = todeg(acos(cgs))

      return
      end

c-----------------------------------------------------------------------

      subroutine reciprocal_to_real
     > (ast,bst,cst,alst,best,gast, a,b,c,al,be,ga)
c
c --- Compute the real cell-parameters
c     from the reciprocal parameters.
c
      vst = vol (ast,bst,cst,torad(alst),torad(best),torad(gast))
      a = bst*cst*sin(torad(alst)) / vst
      b = ast*cst*sin(torad(best)) / vst
      c = ast*bst*sin(torad(gast)) / vst
      al = acos ( (cos(torad(best))*cos(torad(gast))-cos(torad(alst))) /
     >            (sin(torad(best))*sin(torad(gast))) )
      be = acos ( (cos(torad(alst))*cos(torad(gast))-cos(torad(best))) /
     >            (sin(torad(alst))*sin(torad(gast))) )
      ga = acos ( (cos(torad(alst))*cos(torad(best))-cos(torad(gast))) /
     >            (sin(torad(alst))*sin(torad(best))) )
      al = todeg(al)
      be = todeg(be)
      ga = todeg(ga)

      return
      end

c----------------------------------------------------------------------- 

      subroutine solve_cell_parameters
     >    (ludisp,h,k,l,qo,bastab,isyst,
     >     g11,g22,g33,g12,g13,g23, iok)
c
c --- this routine solve the unknown cell-parameters (if possible)
c
      integer   h,k,l,bastab
      dimension h(1),k(1),l(1),qo(1),bastab(1)
      character ch*1
      integer   h2k2l2
      real      amat(6,6),bvec(6)

      iok = 0
c
c --- cubic
c
      if (isyst .eq. 1) then
         i = bastab(1)
         h2k2l2 = h(i)*h(i) + k(i)*k(i) + l(i)*l(i)
         if (h2k2l2 .gt. 0) then 
            g11 = qo(i)/real(h2k2l2)
            g22 = g11
            g33 = g11
            g12 = 0.0
            g13 = 0.0
            g23d = 0.0
            iok = 1
         else
            call impossible_cell (ludisp)
         endif
c
c --- trigonal
c
      elseif (isyst .eq. 2) then
c
c --- tetragonal
c
      elseif (isyst .eq. 3) then
         do irow = 1,2
            amat(irow,1) = real(h(bastab(irow))*h(bastab(irow))+
     >                          k(bastab(irow))*k(bastab(irow)))
            amat(irow,2) = real(l(bastab(irow))*l(bastab(irow)))
            bvec(irow) = qo(bastab(irow))
         enddo
         call gaussj(amat,2,bvec,ising)
         if (ising .eq. 1) then
            write (ludisp,'(a,$)') ' Sorry, singular matrix.'
            call getchar(ch)
            return
         else
            if (bvec(1) .gt. 0.0 .and. bvec(2) .gt. 0.0) then
               g11 = bvec(1)
               g22 = g11
               g33 = bvec(2)
               g12 = 0.0
               g13 = 0.0
               g23 = 0.0
               iok = 1
            else
               call impossible_cell (ludisp)
            endif
         endif
c
c --- hexagonal
c
      elseif (isyst .eq. 4) then
         do irow = 1,2
            amat(irow,1) = real(h(bastab(irow))*h(bastab(irow))+
     >                          h(bastab(irow))*k(bastab(irow))+
     >                          k(bastab(irow))*k(bastab(irow)))
            amat(irow,2) = real(l(bastab(irow))*l(bastab(irow)))
            bvec(irow) = qo(bastab(irow))
         enddo
         call gaussj(amat,2,bvec,ising)
         if (ising .eq. 1) then
            write (ludisp,'(a,$)') ' Sorry, singular matrix.'
            call getchar(ch)
            return
         else
            if (bvec(1) .gt. 0.0 .and. bvec(2) .gt. 0.0) then
               g11 = bvec(1)
               g22 = g11
               g33 = bvec(2)
               g12 = g11
               g13 = 0.0
               g23 = 0.0
               iok = 1
            else
               call impossible_cell (ludisp)
            endif
         endif
c
c --- ortorhombic
c
      elseif (isyst .eq. 5) then
         do irow = 1,3
            amat(irow,1) = real(h(bastab(irow))*h(bastab(irow)))
            amat(irow,2) = real(k(bastab(irow))*k(bastab(irow)))
            amat(irow,3) = real(l(bastab(irow))*l(bastab(irow)))
            bvec(irow) = qo(bastab(irow))
         enddo
         call gaussj(amat,3,bvec,ising)
         if (ising .eq. 1) then
            write (ludisp,'(a,$)') ' Sorry, singular matrix.'
            call getchar(ch)
            return
         else
            if (bvec(1) .gt. 0.0 .and.
     >          bvec(2) .gt. 0.0 .and.
     >          bvec(3) .gt. 0.0 ) then
               g11 = bvec(1)
               g22 = bvec(2)
               g33 = bvec(3)
               g12 = 0.0
               g13 = 0.0
               g23 = 0.0
               iok = 1
            else
               call impossible_cell (ludisp)
            endif
         endif
c
c --- monoclinic
c
      elseif (isyst .eq. 6) then
         do irow = 1,4
            amat(irow,1) = real(h(bastab(irow))*h(bastab(irow)))
            amat(irow,2) = real(k(bastab(irow))*k(bastab(irow)))
            amat(irow,3) = real(l(bastab(irow))*l(bastab(irow)))
            amat(irow,4) = real(h(bastab(irow))*l(bastab(irow)))
            bvec(irow) = qo(bastab(irow))
         enddo
         call gaussj(amat,4,bvec,ising)
         if (ising .eq. 1) then
            write (ludisp,'(a,$)') ' Sorry, singular matrix.'
            call getchar(ch)
            return
         else
            if (bvec(1) .gt. 0.0 .and.
     >          bvec(2) .gt. 0.0 .and.
     >          bvec(3) .gt. 0.0 ) then
               g11 = bvec(1)
               g22 = bvec(2)
               g33 = bvec(3)
               g12 = 0.0
               g13 = bvec(4)
               g23 = 0.0
               iok = 1
            else
               call impossible_cell (ludisp)
            endif
         endif
c
c --- triclinic
c
      elseif (isyst .eq. 7) then
         do irow = 1,6
            amat(irow,1) = real(h(bastab(irow))*h(bastab(irow)))
            amat(irow,2) = real(k(bastab(irow))*k(bastab(irow)))
            amat(irow,3) = real(l(bastab(irow))*l(bastab(irow)))
            amat(irow,4) = real(h(bastab(irow))*k(bastab(irow)))
            amat(irow,5) = real(h(bastab(irow))*l(bastab(irow)))
            amat(irow,6) = real(k(bastab(irow))*l(bastab(irow)))
            bvec(irow) = qo(bastab(irow))
         enddo
         call gaussj(amat,6,bvec,ising)
         if (ising .eq. 1) then
            write (ludisp,'(a,$)') ' Sorry, singular matrix.'
            call getchar(ch)
            return
         else
            if (bvec(1) .gt. 0.0 .and.
     >          bvec(2) .gt. 0.0 .and.
     >          bvec(3) .gt. 0.0 ) then
               g11 = bvec(1)
               g22 = bvec(2)
               g33 = bvec(3)
               g12 = bvec(4)
               g13 = bvec(5)
               g23 = bvec(6)
               iok = 1
            else
               call impossible_cell (ludisp)
            endif
         endif

      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine impossible_cell (ludisp)

      character ch*1

      write(ludisp,'(a)') ' Not possible to solve for cell parameters.'
      write(ludisp,'(a)') ' Press RETURN to continue...'
      call getchar(ch)

      return
      end

c-----------------------------------------------------------------------

      subroutine write_base_error_at_23(ludisp)

      character ch*1

      write(ludisp,'(a)') 
     >' You must define at least one',
     >' baseline before this command works.'
      write(ludisp,'(a)') ' Press RETURN to continue...'
      call getchar(ch)

      return
      end

c-----------------------------------------------------------------------

      subroutine set_base_lines(lukeys,ludisp,n,baseline,ibase,nline)

      dimension ibase(1)
      integer   itemp(6)
      character ch*1
      logical   baseline(20)

      if (n .eq. 1) then
         write(ludisp,'(a,$)') 'Enter line number for baseline: '
         read(lukeys,*,iostat=istat) itemp(1)
      else
         write(ludisp,'(a,i1,a,$)') 
     >   'Enter line numbers for ',n,' baselines: '
         read(lukeys,*,iostat=istat) (itemp(i),i=1,n)
      endif
      if (istat .eq. 0) then
         if (n .eq. 1) then
            ibase(1) = itemp(1)
            write(ludisp,'(a,i1)') ' Baseline used = ',ibase(1)
         else
            do i = 1,n
               ibase(i) = itemp(i)
            enddo
            write(ludisp,'(a,6i2)') 
     >      ' Baselines used = ',(ibase(j),j=1,n)
         endif
      endif
      do i = 1,nline
         baseline(i) = .false.
      enddo
      do i = 1,n
         baseline(ibase(i)) = .true.
      enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine do_graphical_investigation

      include 'puder.cmn'

      write(ludisp,'(a)') 
     >' Module for graphic presentation not ready yet...'

      return
      end

c-----------------------------------------------------------------------

      subroutine search_2d_zone (cmdpar)

      include 'puder.cmn'

      character cmdpar*80, str*80

c --- decode the six arguments for min and max values of 
c     the miller indexes of the three base lines

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip1,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding first parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif

c --- second element

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip2,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding secod parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif

c --- third element

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip3,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding third parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif

c --- fourth element

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip4,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding fourth parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif

c --- fifth element

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip5,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding fifth parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif

c --- sixth element

      if (cmdpar .ne. ' ') then
         call move_first_element (cmdpar, str)       
         call get_int(str,ip6,istat)
         if (istat .ne. 0) then
            if (cmdpar .eq. ' ') then
               write(ludisp,'(a)') 
     >         ' Error decoding sixth parameter as an integer...'
               return
            endif
         endif
      else
         write (ludisp,'(a)') ' Need six index limits for 2dsearch!'
         return
      endif
************************************************************************
      write (ludisp,'(a,6i5)') ' Indexlimits = ',ip1,ip2,ip3,ip4,ip5,ip6

      inegsol = 0
      ipossol = 0
      do ih1 = ip1, ip2
        do ik1 = ip3, ip4
          do il1 = ip5, ip6
            do ih2 = ip1, ip2
              do ik2 = ip3, ip4
                do il2 = ip5, ip6
                  do ih3 = ip1, ip2
                    do ik3 = ip3, ip4
                      do il3 = ip5, ip6
c-----------------------
                        id = idet3 (ih1*ih1, ik1*ik1, ih1*ik1,
     >                              ih2*ih2, ik2*ik2, ih2*ik2,
     >                              ih3*ih3, ik3*ik3, ih3*ik3)
                        if (id .ne. 0) then
                           if (id .lt. 0) then
                              inegsol = inegsol+1
                           elseif (id .gt. 0) then
                              ipossol = ipossol+1
                           endif
                        endif
c-----------------------
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo

      write(ludisp,'(a,i10)') ' Positive combinations = ',ipossol
      write(ludisp,'(a,i10)') ' Negative combinations = ',inegsol

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor(str)

c      include 'puder.cmn'' 
      character str*80,curr*80, crystal_system*80
c
c --- check parameter string
c
      if (str .eq. ' ') then
         write(*,'(a)') ' '
         write(*,'(a)') 'Parameters should be: System, Hmax, Kmax, Lmax'
         write(*,'(a)') ' '
         write(*,'(a)') 'System = CUB, TRIG, TET, HEX, ORT, MON or TRIC'
         write(*,'(a)') ' '
         write(*,'(a)') '(H,K,L)max should be small integers... '
         write(*,'(a)') ' '
         return
      endif
c
c --- read parameters from the parameters string.
c         
      call move_first_element (str, crystal_system)
      call upline(crystal_system)

      istat_hmax  = 1
      istat_kmax  = 1
      istat_lmax  = 1
      call move_first_element (str, curr)
      if (curr .ne. ' ') then
         call get_int(curr,itemp_hmax,istat_hmax)
      endif
      if (str .ne. ' ') then
         call move_first_element (str, curr)
         if (curr .ne. ' ') then
            call get_int(curr,itemp_kmax,istat_kmax)
         endif
      endif
      if (str .ne. ' ') then
         call move_first_element (str, curr)
         if (curr .ne. ' ') then
            call get_int(curr,itemp_lmax,istat_lmax)
         endif
      endif
c
c --- make an attempt in the selected crystal system
c
      if (crystal_system .eq. 'CUB') then
         call ab_initio_treor_cub(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'TRIG') then
         call ab_initio_treor_trig(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'TET') then
         call ab_initio_treor_tet(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'HEX') then
         call ab_initio_treor_hex(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'ORT') then
         call ab_initio_treor_ort(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'MON') then
         call ab_initio_treor_mon(itemp_hmax,itemp_kmax,itemp_lmax)

      elseif (crystal_system .eq. 'TRIC') then
         call ab_initio_treor_tric(itemp_hmax,itemp_kmax,itemp_lmax)

      else
         write(*,'(a)') 'Not known crystal system.'
      endif

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_cub(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32

      write(*,'(a)') 'Not ready yet...'

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_trig(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32

      write(*,'(a)') 'Not ready yet...'

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_tet(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32

      write(*,'(a)') 'Not ready yet...'

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_hex(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32

      write(*,'(a)') 'Not ready yet...'

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_ort(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32
c
c --- generate possible indexes for each line up to some max limit
c
      write(*,'(a,3i5)') 'Max index: ',i1max,i2max,i3max
      
      n = 0
      do i1 = 0,i1max
         do i2 = 0,i2max
            do i3 = 0,i3max
               if (i1.ne.0.or.i2.ne.0.or.i3.ne.0.and.n.lt.1000) then
                 n = n+1
                 h(n) = i1
                 k(n) = i2
                 l(n) = i3
                 write(*,'(i5,a,3i5)')n,':',i1,i2,i3
               else
                 if (n.ge.1000) then
                   write(*,*)
     >             'Program error, ', 
     >             'Redimension arrays in module AB_INITIO_TREOR_ORT.'
                   return
                 endif
               endif
            enddo
         enddo
      enddo
c
c --- generate base line combinations
c
      nok = 0
      do i1 = 1,n
         h12 = h(i1)*h(i1)
         k12 = k(i1)*k(i1)
         l12 = l(i1)*l(i1)
         do i2 = 1,n
            h22 = h(i2)*h(i2)
            k22 = k(i2)*k(i2)
            l22 = l(i2)*l(i2)
            do i3 = 1,n
               h32 = h(i3)*h(i3)
               k32 = k(i3)*k(i3)
               l32 = l(i3)*l(i3)
       if (idet3(h12,k12,l12,h22,k22,l22,h32,k32,l32) .ne. 0) then
          nok = nok+1

       endif
            enddo
         enddo
      enddo
      write(*,'(a,i10)') 'Nr. of det .ne. 0: ',nok

      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_mon(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h1l1
      integer h22,k22,l22,h2l2
      integer h32,k32,l32,h3l3
      integer h42,k42,l42,h4l4
c
c --- generate possible indexes for each line up to some max limit
c
      write(*,'(a,3i5)') 'Index limits (min,max): ',
     > 0,i1max,0,i2max,-i3max,i3max
      
      n = 0
      do i1 = 0,i1max
         do i2 = 0,i2max
            do i3 = -i3max,i3max
               if (i1.ne.0.or.i2.ne.0.or.i3.ne.0.and.n.lt.1000) then
                 n = n+1
                 h(n) = i1
                 k(n) = i2
                 l(n) = i3
                 write(*,'(i5,a,3i5)')n,':',i1,i2,i3
               else
                 if (n.ge.1000) then
                   write(*,*)
     >             'Program error, ', 
     >             'Redimension arrays in module AB_INITIO_TREOR_MON.'
                   return
                 endif
               endif
            enddo
         enddo
      enddo
c
c --- generate base line combinations
c
      nok = 0
      do i1 = 1,n
         h12  = h(i1)*h(i1)
         k12  = k(i1)*k(i1)
         l12  = l(i1)*l(i1)
         h1l1 = h(i1)*l(i1)
         do i2 = 1,n
            h22  = h(i2)*h(i2)
            k22  = k(i2)*k(i2)
            l22  = l(i2)*l(i2)
            h2l2 = h(i2)*l(i2)
            do i3 = 1,n
               h32  = h(i3)*h(i3)
               k32  = k(i3)*k(i3)
               l32  = l(i3)*l(i3)
               h3l3 = h(i3)*l(i3)
               do i4 = 1,n
                  h42  = h(i4)*h(i4)
                  k42  = k(i4)*k(i4)
                  l42  = l(i4)*l(i4)
                  h4l4 = h(i4)*l(i4)
c234567
                  if (idet4(h12,k12,l12,h1l1,
     >                      h22,k22,l22,h2l2,
     >                      h32,k32,l32,h3l3,
     >                      h42,k42,l42,h4l4) .ne. 0) then
                     nok = nok+1
                  endif
               enddo
            enddo
         enddo
      enddo
      write(*,'(a,i10)') 'Nr. of det .ne. 0: ',nok


      return
      end

c-----------------------------------------------------------------------

      subroutine ab_initio_treor_tric(i1max,i2max,i3max)

c      include 'puder.cmn'' 
      integer h(1000),k(1000),l(1000)
      integer h12,k12,l12,h22,k22,l22,h32,k32,l32

      write(*,'(a)') 'Not ready yet...'

      return
      end

