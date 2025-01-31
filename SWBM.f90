 PROGRAM SWBM
  
! Shasta valley soil water budget model (shvWBM)
! 
! *****************************************************************************
! Input and Output is in Metric units:
! 
!                        [L]   =  meters
!                        [L^2] =  squared meters
!                        [L^3] =  cubic meters
!                        [T]   =  days
  
! subwn : watershed name
!	    : 1= Big_spring, 2= Streambot, 3= Julien, 4= Oregon_slough, 5=Yreka, 6=Bunton_hollow, 7 =Boles
!	    : 8=Garrick, 9=Lake_shastina, 10=Juniper_flat, 11=Whaleback_sheep, 12=Whitney, 13=Upper_willow
!	    : 14=Parks, 15=Eddy, 16=Lower_willow, 17=Upper_little_shasta, 18=Davis_gulch, 19=Middle_little_shasta
!	    : 20=Lower_Little_shasta, 21=Grass_lake, 22=Spring_creek_hart, 23=North_gate

! landuse    : Alfalfa 1, Deciduous 2, Grain 3, Other Field crops (corn) 4, Pasture 5, Truck crops (Tomato) 6, Vineyard 7, ET/no_IRRIG 8, no_irrg/no_ET 9
! irr_type   : Wild Flooding 1, Side roll sprinkler 2, Border Strip 3, Hand move sprinkler 4 centerpivot 5, 
!            : Surface drip 6, 555= n* in the DWR categories and should be non irrigated, 999= Unknown irrig
! irrigation efficiency coefficients: irreff_flood, irreff_sprink, irreff_cp
! area       : Area of each polygon
! watersource: SW=1,  MIX=2, GW=3, Uknown =4 or 999, Reclaimed =5
! whc        : water holding capacity
! Water source unknown (999)-> Groundwater 
! Irrigation type unknown (999)-> Wheel Line
! Water source dry or sub or n* (555) with any irrig type -> ET_NoIrr

  USE define_poly
  USE irrigationmodule
  USE outputmodule
  
  IMPLICIT NONE

  INTEGER  :: nmonth, numdays, imonth,jday,i,im,ip, nrows, ncols, dummy, nsegs, n_wel_param, num_daily_out, unit_num, num_MAR_fields
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: zone_matrix, no_flow_matrix, output_zone_matrix, Discharge_Zone_Cells
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: MAR_fields, ip_daily_out
  DOUBLE PRECISION :: pcp, MAR_vol, rnf, total_pcp, base_curve_number, irrn 
  REAL :: Total_Ref_ET, precip
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow, max_MAR_field_rate, moisture_save
  REAL :: start, finish
  INTEGER, DIMENSION(0:11)  :: nday
  CHARACTER(9) :: param_dummy
  CHARACTER(10)  :: SFR_Template, scenario, suffix
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) :: daily_out_name
  INTEGER, DIMENSION(31) :: ET_Active
  LOGICAL :: MAR_active, ILR_active, daily_out_flag
  DOUBLE PRECISION :: rpcp

  call cpu_time(start)
  DATA nday / 30,31,30,31,31,28,31,30,31,30,31,31 /            ! The months are shifted by one because the first index location is zero due to use of MOD function
  open (unit=800, file='SWBM_log.rec')                         ! Open record file for screen output
  rpcp = 0.
  Total_Ref_ET = 0.
  
  open(unit=10, file='general_inputs.txt', status='old')
  read(10, *) npoly, total_n_wells, nmonth, nrows, ncols, RD_Mult, SFR_Template, scenario

  if (trim(scenario)=='basecase' .or. trim(scenario)=='Basecase' .or. trim(scenario)=='BASECASE') then            ! Set logicals for Scenario type
    MAR_active=  .FALSE.  
    ILR_active = .FALSE.
  else if(trim(scenario)=='MAR' .or. trim(scenario)=='mar') then
    MAR_active=  .TRUE. 
    ILR_active = .FALSE.
  else if (trim(scenario)=='ILR' .or. trim(scenario)=='ilr') then
    MAR_active=  .FALSE.
  	ILR_active = .TRUE.
  else if (trim(scenario)=='MAR_ILR' .or. trim(scenario)=='mar_ilr') then
  	 MAR_active=  .TRUE.       
     ILR_active = .TRUE.    
  else if(trim(scenario).ne.'basecase' .or. trim(scenario).ne.'Basecase' .or. trim(scenario).ne.'BASECASE' &      ! Exit program if incorrect scenario type
         .or. trim(scenario).ne.'MAR' .or. trim(scenario).ne.'mar' &
         .or. trim(scenario).ne.'ILR' .or. trim(scenario).ne.'ilr' &
         .or. trim(scenario).ne.'MAR_ILR' .or. trim(scenario).ne.'mar_ilr' ) then
    write(*,*)'Unknown scenario input in general_inputs.txt'
    write(800,*)'Unknown scenario input in general_inputs.txt'
    call EXIT
  end if
    
  write(*,'(2a10)')'Scenario: ',trim(scenario)
  write(800,'(2a10)')'Scenario: ',trim(scenario)
  SFR_Template = TRIM(SFR_Template)
  write(*,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(800,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(*,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "total_n_wells", total_n_wells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult
  write(800,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "total_n_wells", total_n_wells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult 
  close (10)
  open (unit=536, file="well_template.txt", status="old")     
  read(536,*) ! Read heading line into nothing
  read(536,*)param_dummy,n_wel_param  ! read in number of well parameters (for printing later)
  close(536)
  
  ALLOCATE(zone_matrix(nrows,ncols))
  ALLOCATE(no_flow_matrix(nrows,ncols))
  ALLOCATE(output_zone_matrix(nrows,ncols))
  ALLOCATE(Discharge_Zone_Cells(nrows,ncols))
  
  ALLOCATE(moisture_save(npoly))  ! Allocate moisture_save array irrespective of MAR_active ########
  moisture_save = 0.               ! Initialize array#####
  
  open(unit=211,file='recharge_zones.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(211,*) zone_matrix
  open(unit=212,file='No_flow_bound.txt',status='old')       ! Read in MODFLOW no flow cell matrix
  read(212,*) no_flow_matrix  
  output_zone_matrix = zone_matrix * no_flow_matrix        ! Create Recharge Zone Matrix with zeros at no flow cells
  open(unit=213, file='SFR_template.txt', status='old')
  read(213,*) dummy,nsegs 
  open(unit=214,file='ET_Cells_DZ.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(214,*) Discharge_Zone_Cells
  close(211)
  close(212)
  close(213)
  close(214)
  
  call READ_KC_IRREFF                                ! Read in crop coefficients and irrigation efficiencies
  call readpoly(npoly, nrows, ncols, output_zone_matrix) ! Read in field info
  call read_well                                     ! Read in well info
  
 
  close(210)
  close(212)
  close(214) 
  
  open(unit=532,file='5daysdeficiency.dat')  
  open(unit=887,file='precip.txt', status = 'old')                    ! Missing data replaced with value obtained from regression using other stations
  open(unit=88,file='ref_et.txt', status = 'old')
  open(unit=79, file='kc_grain.txt', status = 'old')
  open(unit=80, file='kc_alfalfa.txt', status = 'old')
  open(unit=81, file='kc_pasture.txt', status = 'old')
  open(unit=82, file='kc_vineyard.txt', status = 'old')
  open(unit=83, file='kc_tomato.txt', status = 'old')
  open(unit=87, file='kc_corn.txt', status = 'old')
  open(unit=85, file='kc_deciduous.txt', status = 'old')
  open(unit=86, file='kc_fallow.txt', status = 'old')
   

  open(unit=60, file='subwatershed_area_m2.dat')
  write(60,'("Stress_Period big_spring streambot julien oregon_slough yreka bunton_hollow boles garrick lake_shastina & 
  & juniper_flat whaleback_sheep whitney upper_willow parks eddy lower_willow upper_little davis_gulch middle_little &
  & lower_little grass_lake spring_creek_hart north_gate")')

  
  open(unit=61, file='landuse_area_m2.dat')
  write(61,'(" Stress_Period    Alfalfa    Deciduous    Grain     Corn    Pasture    Tomato     Vineyard   ET_NoIrr    noET_NoIrr")')
  open(unit=62, file='landuse_area_m2_detailed.dat')
  write(62, &
     '(" SP A_Irr D_Irr G_Irr C_irr P_Irr  T_irr", &
     "   V_Irr ET_NoIrr noET_NoIrr Water Total")')
  open(unit=63, file='landuse_area_acres_detailed.dat')
  !write(63,'(" SP A_Irr D_Irr G_Irr C_irr P_Irr T_irr V_Irr  ET_NoIrr noET_NoIrr Water Total")')
    write(63, &
     '(" SP D_Irr G_Irr C_irr P_Irr T_irr", &
     "   V_Irr ET_NoIrr noET_NoIrr Water Total")')
  
  open(unit=599, file = 'daily_out.txt', status = 'old')
  read(599,*)num_daily_out, daily_out_flag
  ALLOCATE(ip_daily_out(num_daily_out))
  ALLOCATE(daily_out_name(num_daily_out))
  if (daily_out_flag) then
  	 do i=1, num_daily_out
  	 	 unit_num =  599 + i 
  	   read(599,*)ip_daily_out(i),daily_out_name(i)
  	   daily_out_name(i) = trim(daily_out_name(i)) // '_daily_out.dat'
  	   open(unit=unit_num, file=daily_out_name(i))
  	   write(unit_num,*)'field_id  precip_adj streamflow irrig  well rch moisture  ET',&
  	                    '  actualET  deficiency budget WC8 subwn landuse rotation'    
    end do
  end if
  
  open(unit=900, file='Recharge_Total.dat')
  
  write(900,*)'Total_Recharge_m^3/day  Total_Recharge_m^3' 

  open(unit=901, file='Runoff_Total.dat')    
  write(901,*)'Total_Runoff_m^3/day  Total_Runoff_m^3' 
  
  open(unit=84, file='MF.rch', STATUS = 'REPLACE')
                                                      
  open(unit=91, file='monthly_gw_normalized.dat', STATUS = 'REPLACE')              
  write(91,*)'Monthly groundwater applied to each field normalized to the field area (m)'
  write(91,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  
  open(unit=92, file='monthly_irrig_normalized.dat')
  write(92,*)'Monthly irrigation applied to each field normalized to the field area (m)'
  write(92,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /)   
  
  open(unit=93, file='monthly_pET_normalized.dat')
  write(93,*)'Monthly potential ET for each field normalized to the field area (m)'
  write(93,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=94, file='monthly_recharge_normalized.dat')
  write(94,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(94,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
   open(unit=95, file='monthly_storage_normalized.dat')
  write(95,*)'Storage at the end of the month for each field normalized to the field area (m)'
  write(95,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
    open(unit=96, file='monthly_aET_normalized.dat')
  write(96,*)'Monthly actual ET for each field normalized to the field area (m)'
  write(96,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=97, file='monthly_deficiency_normalized.dat')
  write(97,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(97,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=98, file='monthly_runoff_normalized.dat')
  write(98,*)'Monthly runoff from each field normalized to the field area (m)'
  write(98,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 


  open(unit=200, file='monthly_gw_volume.dat', STATUS = 'REPLACE')              
  write(200,*)'Monthly groundwater volume applied to each field (m^3)'
  write(200,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  open(unit=201, file='monthly_irrig_volume.dat')
  write(201,*)'Monthly irrigation volume applied to each field (m^3)'
  write(201,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /)    
  open(unit=202, file='monthly_pET_volume.dat')
  write(202,*)'Monthly potential ET for each field (m^3)'
  write(202,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=203, file='monthly_recharge_volume.dat')
  write(203,*)'Monthly groundwater recharge from each field (m^3)'
  write(203,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=207, file='monthly_runoff_volume.dat')
  write(207,*)'Monthly runoff from each field (m^3)'
  write(207,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  
  open(unit=204, file='monthly_storage_volume.dat')
  write(204,*)'Storage at the end of the month for each field (m^3)'
  write(204,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=205, file='monthly_aET_volume.dat')
  write(205,*)'Monthly actual ET for each field (m^3)'
  write(205,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=206, file='monthly_deficiency_volume.dat')
  write(206,*)'Monthly groundwater recharge from each field (m^3)'
  write(206,'(a14, 1724i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
                             
  open(unit=530, file='Monthly_Pumping_Volume_By_Well.dat')
  write(530,*)'Monthly Pumping Volume (m^3) by well'
  write(530,'(1724i12)')single_well(:)%well_name
  open(unit=531, file='Monthly_Pumping_Rate_By_Well.dat')
  write(531,*)'Monthly Pumping Rate (m^3/day) by well'
  write(531,'(1724i12)')single_well(:)%well_name
  open(unit=537, file='daily_pumping.dat')
  write(537, *)"Daily pumping volume (m^3) for each well"
  write(537,'(1724i12)')single_well(:)%well_name
  open(unit=538, file='daily_average_RCH.dat')
  write(538, *)"Daily weighted averaged recharge caluclated in each well, to be averaged and used in stream depletion"
  open(unit=120, file='ET_Active_Days.dat')                       
  write(120,'("Number of Days ET is Active in each polyon")')    
  open(unit=66, file='streamflow_input.txt', status='old')
  read(66,*)                                                        ! Read Header into nothing
  
  open(unit=101, file='monthly_surfacewater_by_subw.dat')        
  open(unit=102, file='monthly_groundwater_by_subw.dat')      
  open(unit=103, file='monthly_irrigation_by_subw.dat')     
  open(unit=104, file='monthly_pET_by_subw.dat')     
  open(unit=105, file='monthly_aET_by_subw.dat')  
  open(unit=106, file='monthly_recharge_by_subw.dat')  
  open(unit=107, file='monthly_deficiency_by_subw.dat')
  open(unit=108, file='monthly_storage_by_subw.dat')   
  open(unit=109, file='monthly_runoff_by_subw.dat')  

   do i = 101, 109
       write(i,*) 'Stress_Period big_spring streambot julien oregon_slough yreka bunton_hollow boles garrick lake_shastina' // &
                  ' juniper_flat whaleback_sheep whitney upper_willow parks eddy lower_willow upper_little davis_gulch middle_little' // &
                  ' lower_little grass_lake spring_creek_hart north_gate'
   end do
                                                            
     
  open(unit=110, file='monthly_groundwater_by_luse.dat')      
  open(unit=111, file='monthly_irrigation_by_luse.dat')     
  open(unit=112, file='monthly_pET_by_luse.dat')     
  open(unit=113, file='monthly_aET_by_luse.dat')  
  open(unit=114, file='monthly_recharge_by_luse.dat')  
  open(unit=115, file='monthly_deficiency_by_luse.dat')
  open(unit=116, file='monthly_storage_by_luse.dat')  
  open(unit=117, file='monthly_surfacewater_by_luse.dat') 
 open(unit=118, file='monthly_runoff_by_luse.dat')    
  
  do i=110,118
    write(i,*)'Stress_Period  Alfalfa  Deciduous  Grain  Corn  Pasture  Tomato  Vineyard  ET_NoIrr  NoET_NoIrr'
  end do
  
  open (unit=119, file='monthly_water_budget.dat')
  write(119,*)'Stress_Period Precip SW_Irr GW_Irr ET Recharge Runoff Storage'

   !CALL EXECUTE_COMMAND_LINE('copy ET_template.txt MF.ets')
   !CALL EXECUTE_COMMAND_LINE('copy SFR_template.txt MF.sfr')
   !CALL EXECUTE_COMMAND_LINE('copy well_template.txt MF.wel')
   !if (SFR_Template=='UCODE') then 
     !CALL EXECUTE_COMMAND_LINE('copy SFR_UCODE_JTF.txt MF_SFR.jtf')
   !elseif (SFR_Template=='PEST') then
   	 !CALL EXECUTE_COMMAND_LINE('copy SFR_PEST_TPL.txt MF_SFR.tpl')
   !else
     	!write(*,*)'Invalid Template File Format Variable in general_inputs.txt'
     	!write(800,*)'Invalid Template File Format Variable in general_inputs.txt'
     	!CALL EXIT
   !end if
   
   open (unit=220, file='Drains_m3day.txt')
   read(220,*)param_dummy     ! Read header comment line to dummy character string 
   poly%irr_flag = 0          ! Initialize irrigation flag array
   do im=1, nmonth            ! Loop over each month
     imonth=MOD(im,12)        ! Create repeating integers for months (Oct=1, Nov=2, ..., Aug=11, Sep=0)
     numdays = nday(imonth)   ! Number of days in the current month
     call zero_month                                 ! Zero out monthly accumulated volume
     if (imonth==1) call zero_year                             ! If October, Zero out yearly accumulated volume
     if (im==1) then 
       call do_rotation(im)                   ! populate initial poly%rotation values \
     else if (imonth==4 .and. im.ne.4) then
       call do_rotation(im)	                 ! Rotate alfalfa/grain in January, except for first year since rotation happened in October   
     end if                   
     call calc_area(im)                ! Calculate area for each month due to changing alfalfa/grain
     call read_streamflow(numdays)     ! Read in streamflow inputs
     write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', nday(imonth)
     write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', nday(imonth)
     call zero_month                                 ! Zero out monthly accumulated volume
     if (imonth==1) then                             ! If October:
       call zero_year                                ! Zero out yearly accumulated volume
     end if    
     !read(220,*)drain_flow(im)                       ! Read drain flow into array         
     do jday=1, nday(imonth)                         ! Loop over days in each month
       if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
       daily%ET_active  = 0                                 ! Reset ET active counter
       daily%irrigation = 0.                                ! Reset daily irrigation value to zero
       daily%well       = 0.                                ! Reset daily pumping value to zero
       daily%pcp  = 0.                                ! Reset daily effective precip value to zero
       daily%evapotrasp = 0.                                ! Reset daily ET value to zero
       daily%recharge   = 0.
       daily%runoff   = 0. 	   
       read(88,*) REF_ET
       Total_Ref_ET = Total_Ref_ET + REF_ET                 
       read(79,*) kc_grain
       read (80,*)kc_alfalfa
       read(81,*)kc_pasture
	   read(82,*)kc_vineyard
	   read(83,*)kc_tomato
	   read(87,*)kc_corn
	   read(85,*)kc_deciduous
	   read(86,*)kc_fallow
       read(887,*) precip
         rpcp=precip							 

		   do ip=1, npoly      
	       if (imonth==3 .and. jday==31 .and. ip==npoly) then ! If last day of the year, set irrigation flags and logical to zero
			     poly%irr_flag = 0         
			     irrigating = .false.           
		       print*, 'Irrigation Logical Reset'
		       write(800,*)'Irrigation Logical Reset'
		       call Update_Irr_Type(im)
		       print*, 'Irrigation Type Updated'    
           write(800,*)'Irrigation Type Updated'
		     end if
             call RUNOFF(ip, rpcp, irrn, base_curve_number, rnf, jday, imonth)
			 
	         call IRRIGATION(ip, imonth, jday, rpcp)
	   
	         call RECHARGE(ip,dble(rpcp),jday, imonth, moisture_save)
		     call deficiency_check(ip, imonth, jday)  
             	 
       enddo              ! End of polygon loop
       if (daily_out_flag) call daily_out(num_daily_out,ip_daily_out, rpcp)              ! Print Daily Output for Selected Fields
       call pumping(ip, jday, total_n_wells, npoly)   ! Stream depletion subroutine
		   call monthly_SUM      ! add daily value to monthly total (e.g., monthly%irrigation = monthly%irrigation + daily%irrigation)
       call annual_SUM       ! add daily value to yearly total (e.g., yearly%irrigation = yearly%irrigation + daily%irrigation)
       if (jday==numdays) then
           call SFR_streamflow(numdays, imonth)         ! Convert remaining surface water to SFR inflows at end of the month	
       end if
       enddo             ! End of day loop
       jday = jday -1 ! reset jday to number of days in month (incremented to ndays +1 in do loop above)
       call convert_length_to_volume
       call monthly_out_by_field(im)
       call monthly_pumping(im, jday, total_n_wells)
		   call ET_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix, Total_Ref_ET,Discharge_Zone_Cells,npoly)
		   Total_Ref_ET = 0.  ! Reset monthly Average ET
		   call recharge_out_MODFLOW(im,imonth,nday,nrows,ncols,output_zone_matrix)
       call monthly_volume_out		   
       call write_MODFLOW_WEL(im,imonth,total_n_wells,n_wel_param)       
   enddo                  ! End of month loop
   close(84)
   close(60)
   close(61)		
   close(91)
   close(92)
   close(93)
   close(94)
   close(95)
   close(96)
   close(97)
   close(98)
   call cpu_time(finish)
   write(*,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
   write(800,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
   
   END PROGRAM SWBM

  !*******************************************************************************************	
  SUBROUTINE deficiency_check(ip,imonth,iday)
  use define_poly

  integer :: ip, imonth, iday
  integer, save :: year = 0

  if ((poly(ip)%landuse  == 1) .and. (poly(ip)%WC8.ne.0.)) then    ! if alfalfa

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return
   
    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
   
    ! otherwise...
   
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif
	
  elseif ((poly(ip)%landuse  == 2) .and. (poly(ip)%WC8.ne.0.)) then    ! if Deciduous 

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return
   
    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
   
    ! otherwise...
   
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif
 
  elseif ((poly(ip)%landuse  == 3) .and. (poly(ip)%WC8.ne.0.)) then    ! if grain 

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return

    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
    
    ! otherwise...
    
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif 

  elseif ((poly(ip)%landuse  == 4) .and. (0.5*poly(ip)%WC8.ne.0.)) then    ! if corn

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return

    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
    
    ! otherwise...
    
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif 



  elseif ((poly(ip)%landuse  == 5) .and. (poly(ip)%WC8.ne.0.)) then    ! if pasture

    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return

    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
    
    ! otherwise...
    
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif  
	
 
  elseif ((poly(ip)%landuse  == 6) .and. (poly(ip)%WC8.ne.0.)) then    ! if Truck crops, tomato
    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return
   
    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
   
    ! otherwise...
   
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif
	
  elseif ((poly(ip)%landuse  == 7) .and. (poly(ip)%WC8.ne.0.)) then    ! if Vineyard 
    if ( (imonth == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return
   
    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
   
    ! otherwise...
   
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'poly',ip,'date', iday, imonth, year
      daily(ip)%daydef = -100
    endif
  
  endif

  return
  END SUBROUTINE deficiency_check 
  
! *******************************************************************************************	

SUBROUTINE RUNOFF(ip, rpcp, irrn, base_curve_number, rnf, jday, imonth)
  USE define_poly

  INTEGER, INTENT(in) :: ip, jday, imonth
  DOUBLE PRECISION, INTENT(in) :: rpcp           ! Daily precipitation in meters
  DOUBLE PRECISION, INTENT(INOUT) :: irrn        ! Total Irrigation water
  DOUBLE PRECISION, INTENT(in) :: base_curve_number ! Base Curve Number, average condition
  DOUBLE PRECISION, INTENT(out) :: rnf           ! Daily runoff

  DOUBLE PRECISION :: S, Ia, CN, fc, total_pcp   ! S = Potential maximum retention, Ia = Initial abstraction
  DOUBLE PRECISION, PARAMETER :: CN_min = 30.0d0, CN_max = 100.0d0
  DOUBLE PRECISION :: CN_AMC_I, CN_AMC_II, CN_AMC_III, adjustment_ratio
  DOUBLE PRECISION :: cumulative_pcp_5day
  DOUBLE PRECISION, DIMENSION(5), SAVE :: past_precipitation = 0.0
  INTEGER :: i

  rnf = 0.0  ! Initialize daily runoff to zero

  irrn = daily(ip)%irrigation
  total_pcp = rpcp + irrn

  fc = poly(ip)%WC8 ! Field capacity from the 'poly' array

  ! Validate curve number range
  IF (poly(ip)%CN < CN_min .OR. poly(ip)%CN > CN_max) THEN
    PRINT *, 'Error: poly(ip)%CN out of range'
    STOP
  END IF

  ! Update past precipitation array (rolling update)
  DO i = 5, 2, -1
    past_precipitation(i) = past_precipitation(i-1)
  END DO
  past_precipitation(1) = rpcp

  ! Calculate 5-day cumulative precipitation in meters
  cumulative_pcp_5day = SUM(past_precipitation)

  ! Adjust curve numbers for different AMC conditions
  CN_AMC_II = poly(ip)%CN + 5.0                    ! Average condition (AMC II)
  CN_AMC_I = CN_AMC_II / (2.281 - 0.01281 * CN_AMC_II)
  CN_AMC_III = CN_AMC_II / (0.427 + 0.00573 * CN_AMC_II)

  ! Adjust CN based on the dryness or wetness of the day compared to the 5-day trend
  IF (cumulative_pcp_5day < 0.00043) THEN ! 1/3 of the average Precip
    CN = CN_AMC_I
  ELSEIF (cumulative_pcp_5day > 0.00043  .AND. cumulative_pcp_5day <= 0.00065) THEN
    CN = CN_AMC_II
  ELSE
    CN = CN_AMC_III
  END IF

  CN = MAX(CN_min, MIN(CN, CN_max))
  S = (25.40 / CN) - 0.254  ! Retention parameter (S) based on curve number (CN)
  Ia = 0.2 * S  ! Initial abstraction (Ia)

  ! Runoff calculation
  IF (total_pcp > Ia) THEN
    rnf = ((total_pcp - Ia)**2) / (total_pcp + 0.8 * S)
  ELSE
    rnf = 0.0
  END IF

  ! Ensure runoff does not exceed total precipitation
 rnf = MAX(0.0, MIN(rnf, total_pcp))

  ! Update outputs
  daily(ip)%runoff = rnf
  daily(ip)%cumulative_pcp_5day = cumulative_pcp_5day
END SUBROUTINE RUNOFF


!****************************************************************************

SUBROUTINE RECHARGE(ip, rpcp, jday, imonth, moisture_save)
    USE define_poly

    IMPLICIT NONE

    INTEGER :: ip, jday, imonth
    DOUBLE PRECISION :: RCH, rpcp, rnf, rd,fc, p
    REAL, DIMENSION(npoly), INTENT(inout) :: moisture_save


    ! Root depth multiplier
    SELECT CASE (poly(ip)%landuse)
        CASE (1)   ! Alfalfa
            rd = 1.83
			p = 0.72
        CASE (2)   ! Deciduous
            rd = 1.0
			p = 0.65
        CASE (3)   ! Grain
            rd = 0.35
			p = 0.62
        CASE (4)   ! Corn
            rd = 1.2
			p = 0.62
        CASE (5)   ! Pasture
            rd = 1.0
			p = 0.75
        CASE (6)   ! Truck Crops (Tomato)
            rd = 0.81
			p = 0.63
        CASE (7)   ! Vineyard (Grapes)
            rd = 1.3
			p = 0.65
        CASE DEFAULT
            rd = 1.0  ! Default root depth multiplier if crop type is unspecified
			p = 0.62
    END SELECT

    ! Initialize or assign field capacity (FC) and permanent wilting point (PWP)
      fc = rd * poly(ip)%WC8

    IF (poly(ip)%landuse /= 10) THEN  ! Exclude water landuse type (no recharge consideration)
        daily(ip)%actualET = MIN(daily(ip)%evapotrasp, before(ip)%moisture + rpcp + daily(ip)%irrigation - daily(ip)%runoff)
        daily(ip)%deficiency = daily(ip)%evapotrasp - daily(ip)%actualET - daily(ip)%runoff
        IF (daily(ip)%actualET > 0) THEN
            daily(ip)%ET_active = 1  ! Set ET flag to 1 if ET is active that day
        ENDIF

        ! Calculate recharge based on landuse

        RCH = MAX(0., (before(ip)%moisture + rpcp + daily(ip)%irrigation - daily(ip)%actualET - daily(ip)%runoff - fc))
        daily(ip)%recharge = RCH

        daily(ip)%moisture = MAX(0., before(ip)%moisture + rpcp + daily(ip)%irrigation - daily(ip)%actualET - daily(ip)%recharge - daily(ip)%runoff)
        CALL waterbudget(ip, rpcp)

        before(ip)%moisture = daily(ip)%moisture
        daily(ip)%change_in_storage = rpcp + daily(ip)%irrigation - daily(ip)%actualET - daily(ip)%recharge - daily(ip)%runoff
    END IF

END SUBROUTINE RECHARGE

!****************************************************************************

  SUBROUTINE waterbudget(ip, rpcp)

  use define_poly

  implicit none

  INTEGER :: ip
  DOUBLE PRECISION    :: rpcp
!  DOUBLE PRECISION:: PRECIP, RCH

  
  daily(ip)%budget = daily(ip)%moisture-before(ip)%moisture+daily(ip)%actualET+daily(ip)%recharge- &
                     rpcp-daily(ip)%irrigation-daily(ip)%runoff      

  END SUBROUTINE 
 