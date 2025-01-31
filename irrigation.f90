MODULE irrigationmodule
  
  use define_poly
  implicit none
  
  DOUBLE PRECISION:: kc_grain, kc_alfalfa, kc_corn, kc_deciduous, kc_vineyard, kc_pasture, kc_tomato, kc_fallow
  DOUBLE PRECISION:: kc_alfalfa_mult, kc_grain_mult, kc_pasture_mult, kc_deciduous_mult, kc_corn_mult, &
                     kc_tomato_mult, kc_vineyard_mult, kc_noirr
  DOUBLE PRECISION:: irreff_flood, side_roll, border_strip, hand_move, center_pivot, surf_drip
  DOUBLE PRECISION :: AV_REF_ET_1a, AV_REF_ET_1b, AV_REF_ET_2, REF_ET
  DOUBLE PRECISION :: monthly_precip_vol
  INTEGER, parameter:: nsubwn = 23 ! shasta valley 
  INTEGER, parameter:: nlanduse = 9! shasta
  LOGICAL :: irrigating
  DOUBLE PRECISION, DIMENSION (1:33) :: SFR_Flows ! 33 Streamflow segments 
  DOUBLE PRECISION, DIMENSION(nsubwn) :: streamflow_in, streamflow_out, sw_irr
  
  contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE read_kc_irreff
  
    open(unit=10,file="irr_eff.txt",status="old")
    read(10,*)irreff_flood, side_roll, border_strip
	read(10,*)hand_move, center_pivot, surf_drip     
    write(*,*) ! Blank Line
    write(*,'(A45)') "Effective Irrigation Efficiencies (IE + SMDF)"
    write(*,'(A8,F4.2)') "Flood = ", irreff_flood
	write(*,'(A22,F4.2)') "Side Roll Sprinkler = ", side_roll
    write(*,'(A15,F4.2)') "Border Strip  = ", border_strip
	write(*,'(A22,F4.2)') "Hand Move Sprinkler  = ", hand_move
    write(*,'(A15,F4.2)') "Center Pivot = ", center_pivot
	write(*,'(A15,F4.2)') "Surface Drip = ", surf_drip
	write(800,*) ! Blank Line
	write(800,'(A45)') "Effective Irrigation Efficiencies (IE + SMDF)"
    write(800,'(A8,F4.2)') "Flood = ", irreff_flood
	write(800,'(A22,F4.2)') "Side Roll Sprinkler = ", side_roll
    write(800,'(A15,F4.2)') "Border Strip  = ", border_strip
	write(800,'(A22,F4.2)') "Hand Move Sprinkler  = ", hand_move
    write(800,'(A15,F4.2)') "Center Pivot = ", center_pivot
	write(800,'(A15,F4.2)') "Surface Drip = ", surf_drip
    close (10)
    
    open(unit=11,file="crop_coeff_mult.txt",status="old")
    read(11,*)kc_alfalfa_mult, kc_deciduous_mult, kc_grain_mult, kc_corn_mult
	read(11,*) kc_pasture_mult, kc_tomato_mult, kc_vineyard_mult, kc_noirr
    close(11)
    write(*,*) ! Blank Line
    write(*,'(A20,F4.2)') "kc_alfalfa_mult = ", kc_alfalfa_mult
	write(*,'(A20,F4.2)') "kc_deciduous_mult = ", kc_deciduous_mult
    write(*,'(A18,F4.2)') "kc_grain_mult = ", kc_grain_mult
	write(*,'(A24,F4.2)') "kc_otherfields_mult = ", kc_corn_mult
    write(*,'(A20,F4.2)') "kc_pasture_mult = ", kc_pasture_mult
    write(*,'(A23,F4.2)') "kc_truckcrops_mult = ", kc_tomato_mult
    write(*,'(A21,F4.2)') "kc_vineyard_mult = ", kc_vineyard_mult
	write(*,'(A11,F4.2)') "kc_noirr = ", kc_noirr
    write(*,*) ! Blank Line
    write(800,*) ! Blank Line
    write(800,'(A20,F4.2)') "kc_alfalfa_mult = ", kc_alfalfa_mult
	write(800,'(A17,F4.2)') "kc_deciduous_mult = ", kc_deciduous_mult
    write(800,'(A18,F4.2)') "kc_grain_mult = ", kc_grain_mult
	write(800,'(A24,F4.2)') "kc_otherfields_mult = ", kc_corn_mult
    write(800,'(A20,F4.2)') "kc_pasture_mult = ", kc_pasture_mult
    write(800,'(A23,F4.2)') "kc_truckcrops_mult = ", kc_tomato_mult
    write(800,'(A21,F4.2)') "kc_vineyard_mult = ", kc_vineyard_mult
	write(800,'(A11,F4.2)') "kc_noirr = ", kc_noirr
    write(800,*) ! Blank Line	
    return
  end subroutine read_kc_irreff
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
SUBROUTINE get_crop_properties(ip)

  INTEGER :: ip
DOUBLE PRECISION :: ks, moisture, fc, TAW, RAW, Dr, rdf
  ! Determine crop-specific root depth and water availability factor
  SELECT CASE (poly(ip)%landuse)
    CASE (1)   ! Alfalfa
       poly(ip)%rd = 1.83
      poly(ip)%p = 0.72
    CASE (2)   ! Deciduous
      poly(ip)%rd = 1.5
      poly(ip)%p = 0.65
    CASE (3)   ! Grain
      poly(ip)%rd = 0.35
      poly(ip)%p = 0.72
    CASE (4)   ! Corn
      poly(ip)%rd = 1.2
      poly(ip)%p = 0.72
    CASE (5)   ! Pasture
      poly(ip)%rd = 1.2
      poly(ip)%p = 0.78
    CASE (6)   ! Truck Crops (Tomato)
      poly(ip)%rd = 0.81
      poly(ip)%p = 0.63
    CASE (7)   ! Vineyard (Grapes)
      poly(ip)%rd = 1.3
      poly(ip)%p = 0.65
    CASE DEFAULT
      poly(ip)%rd = 1.0  ! Default values
      poly(ip)%p = 0.62
  END SELECT

END SUBROUTINE get_crop_properties

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
SUBROUTINE IRRIGATION(ip, imonth, jday, rpcp)
    
    INTEGER :: imonth, jday
    INTEGER, INTENT(in) :: ip
    DOUBLE PRECISION, INTENT(in) :: rpcp
    DOUBLE PRECISION :: ks, moisture, fc, TAW, RAW, Dr, rdf

  ! Update crop properties
  CALL get_crop_properties(ip)

  ! Soil calculations using updated crop properties
  fc = poly(ip)%rd * poly(ip)%WC8
  moisture = daily(ip)%moisture
  TAW = fc
  Dr = MAX(0.0, fc - moisture)
  RAW = poly(ip)%p * TAW
  rdf = MIN(MAX((Dr - RAW) / (TAW - RAW), 0.0), 1.0)

  ! Calculate stress factor (K_s)
  IF (Dr <= RAW) THEN
    ks = 1.0
  ELSE
    ks = MAX(0.0, (TAW - Dr) / (TAW - RAW))
  END IF

    IF (sum(poly%irr_flag) .ge. 200) irrigating = .true. 

    SELECT CASE (poly(ip)%landuse)
        CASE (1)   ! Alfalfa
            daily(ip)%pcp  = rpcp 
            daily(ip)%evapotrasp = REF_ET * kc_alfalfa * kc_alfalfa_mult * ks
            IF ((imonth == 6 .AND. jday >= 15) .OR. (imonth >= 7) .OR. (imonth == 0) .OR. (imonth == 1 .AND. jday <= 15)) THEN
                IF ((daily(ip)%moisture < RAW) .OR. (imonth == 8 .AND. jday >= 15) .OR. (imonth > 8) .OR. irrigating) THEN
                    CALL IRRIGATION_RULESET(imonth, jday, ip)
                END IF
            END IF

        CASE (2)   ! Deciduous
            daily(ip)%pcp  = rpcp 
            daily(ip)%evapotrasp = REF_ET * kc_deciduous*ks
            IF ((imonth == 5 .AND. jday >= 15) .OR. (imonth >= 6) .OR. (imonth == 0) .OR. (imonth == 1 .AND. jday <= 15)) THEN
                IF ((daily(ip)%moisture < RAW) .OR. (imonth == 8 .AND. jday >= 15) .OR. (imonth > 8) .OR. irrigating) THEN
                    CALL IRRIGATION_RULESET(imonth, jday, ip)
                END IF
            END IF
  
        CASE (3)   ! Grain
            daily(ip)%pcp  = rpcp 
            daily(ip)%evapotrasp = REF_ET * kc_grain * kc_grain_mult*ks
            IF ((imonth == 6 .AND. jday >= 16) .OR. (imonth >= 7 .AND. imonth <= 10) .OR. (imonth == 10 .AND. jday <= 10)) THEN
                IF ((daily(ip)%moisture < RAW) .OR. (imonth == 8 .AND. jday >= 15) .OR. (imonth > 8) .OR. irrigating) THEN
                    CALL IRRIGATION_RULESET(imonth, jday, ip)
                END IF
            END IF
         case (4)    ! Corn
           daily(ip)%pcp  = rpcp 
           daily(ip)%evapotrasp = REF_ET * kc_corn * ks
 
		  if ((imonth==6 .and. jday.ge.16 ) .or. (imonth.ge.7 .and. imonth.le.9 ) .or. (imonth==10 .and. jday.le.10)) then  ! If  March 16 - July 10
             if ((daily(ip)%moisture < RAW) &
              .or. (imonth==8 .and. jday.ge.15) .or. (imonth>8) .or. irrigating) then
               call IRRIGATION_RULESET(imonth, jday, ip)
             end if
           end if
 
         case (5)    ! Pasture
           daily(ip)%pcp  = rpcp 
           daily(ip)%evapotrasp = REF_ET * kc_pasture *kc_pasture_mult*ks
 
            IF ((imonth == 6 .AND. jday >= 15) .OR. (imonth >= 7) .OR. (imonth == 0) .OR. (imonth == 1 .AND. jday <= 15)) THEN
                IF ((daily(ip)%moisture < TAW) .OR. (imonth == 8 .AND. jday >= 15) .OR. (imonth > 8) .OR. irrigating) THEN
               call IRRIGATION_RULESET(imonth, jday, ip)
             end if
           end if
 
        case (6)    ! Truck Crops (Tomato)
          daily(ip)%pcp  = rpcp 
          daily(ip)%evapotrasp = REF_ET * kc_tomato * ks

          if ((imonth == 7 .and. jday >= 15) .or. (imonth >= 8)) then
            if ((daily(ip)%moisture < RAW) .or. (imonth == 8 .and. jday >= 15) .or. (imonth > 8) .or. irrigating) then
              call IRRIGATION_RULESET(imonth, jday, ip)
            end if
          end if
  
        case (7)    ! Vineyard (Grapes)
          daily(ip)%pcp  = rpcp 
          daily(ip)%evapotrasp = REF_ET * kc_vineyard * ks
          if ((imonth == 6 .and. jday >= 10) .or. (imonth >= 8)) then
            if ((daily(ip)%moisture < RAW) .or. (imonth == 8 .and. jday >= 15) .or. (imonth > 8) .or. irrigating) then
              call IRRIGATION_RULESET(imonth, jday, ip)
            end if
          end if
        case (8)    ! ET_noIRR, Unclassified Fallow
		    daily(ip)%pcp  = rpcp
            daily(ip)%irrigation= 0.
            daily(ip)%well=0.
            daily(ip)%evapotrasp = REF_ET*kc_fallow*ks
        case (9)    ! noET_noIRR
		daily(ip)%pcp  = rpcp
           daily(ip)%irrigation=0.
           daily(ip)%well=0.      
           daily(ip)%moisture=0.  
           daily(ip)%evapotrasp=0.
                          
        case (10)    ! water landuse type
		
        CASE DEFAULT
            ! Handle any other or unclassified land use types if necessary
            daily(ip)%irrigation = 0.0
            daily(ip)%evapotrasp = 0.0
    END SELECT

END SUBROUTINE IRRIGATION

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE IRRIGATION_RULESET(imonth, jday, ip)
    INTEGER :: imonth, jday, ip
    DOUBLE PRECISION :: ks, moisture, fc, TAW, RAW, Dr, rdf, pcp
    DOUBLE PRECISION :: irr_eff, rpcp, Peff, rnf  ! Efficiency factor and effective precipitation

 ! Get crop-specific properties and soil calculations
  !CALL get_crop_properties(poly(ip)%landuse, poly(ip), daily(ip), rd, p, fc, moisture, TAW, RAW, Dr, rdf)

    poly(ip)%irr_flag = 1  ! Field has started irrigating
    
    ! Select irrigation efficiency based on irrigation type
    SELECT CASE (poly(ip)%irr_type)
        CASE (1)   ! Flood irrigation
            irr_eff = irreff_flood
        CASE (2)   ! Side roll irrigation
            irr_eff = side_roll
        CASE (3)   ! Border strip irrigation
            irr_eff = border_strip
        CASE (4)   ! Hand move irrigation
            irr_eff = hand_move
        CASE (5)   ! Center pivot irrigation
            irr_eff = center_pivot
        CASE (6)   ! Surface drip irrigation
            irr_eff = surf_drip
        CASE DEFAULT
            irr_eff = 0.8  ! Default efficiency if irrigation type is unspecified
    END SELECT

    ! Irrigation amount adjusted by depletion fraction and irrigation efficiency
    daily(ip)%irrigation = MAX(0., (1 / irr_eff) * (daily(ip)%evapotrasp - (pcp-rnf)))
	!daily(ip)%irrigation = Dr/irr_eff
    
    ! Adjust for water sources
    IF (poly(ip)%water_source == 1) THEN  ! Surface-water
        sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%MF_area
        IF (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) THEN 
            sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%MF_area
            daily(ip)%irrigation = 0.0
        END IF
    ELSE IF (poly(ip)%water_source == 3) THEN  ! Groundwater
        daily(ip)%well = daily(ip)%irrigation
    ELSE IF (poly(ip)%water_source == 2) THEN  ! Mixed water source
        sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) + daily(ip)%irrigation * poly(ip)%MF_area
        IF (sw_irr(poly(ip)%subwn) > streamflow_in(poly(ip)%subwn)) THEN 
            sw_irr(poly(ip)%subwn) = sw_irr(poly(ip)%subwn) - daily(ip)%irrigation * poly(ip)%MF_area
            daily(ip)%well = daily(ip)%irrigation
        END IF
    END IF

END SUBROUTINE IRRIGATION_RULESET
 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine do_rotation(im) ! it is not applied/land use map does not have 
  
      integer :: i, im, year
      integer :: rotcycle, ipr 
      integer :: ngrain
    
      ngrain = nrot/8
      year = im/12 + 1 
      rotcycle=mod(year,8)
  
      ipr = 0
      print*, ''
      write(*,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
      write(*,*)'-------------------------------------------------------------'
      write(800,*) ''
      write(800,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
      write(800,*)'-------------------------------------------------------------'
      do i = 1, npoly
        if (poly(i)%landuse == 25) then    
          ipr = ipr + 1
          poly(i)%rotation = 11 ! alfalfa
          if ( ipr>(rotcycle*ngrain) .and. ipr<=((rotcycle+1)*ngrain) ) then
            poly(i)%rotation = 12
          end if
        end if
      enddo
  
  end subroutine do_rotation
   
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine read_streamflow(numdays)
    
    CHARACTER(10) :: date_dummy
	DOUBLE PRECISION :: Big_spring, Streambot, Julien, Oregon_slough, Yreka, Bunton_hollow, Boles, Garrick, Lake_shastina, &		
	                    Juniper_flat, Whaleback_sheep, Whitney, Upper_willow, Parks, Eddy, Lower_willow, Upper_little_shasta, &		
	                    Davis_gulch, Middle_little_shasta, Lower_Little_shasta,	Grass_lake,	Spring_creek_hart,	North_gate
    INTEGER :: numdays
    
    streamflow_in = 0.        ! Reset all streamflow to zero
    sw_irr = 0.               ! Reset surface-water irrigation to zero
    
	 read(66,*)date_dummy,Big_spring, Streambot, Julien, Oregon_slough, Yreka, Bunton_hollow, Boles, Garrick, Lake_shastina, &		
	    Juniper_flat, Whaleback_sheep, Whitney, Upper_willow, Parks, Eddy, Lower_willow, Upper_little_shasta, &		
	    Davis_gulch, Middle_little_shasta, Lower_Little_shasta,	Grass_lake,	Spring_creek_hart,	North_gate
	
	streamflow_in (1)= Big_spring
	streamflow_in (2)= Streambot
	streamflow_in (3)= Julien
	streamflow_in (4)= Oregon_slough
	streamflow_in (5)= Yreka
	streamflow_in (6)= Bunton_hollow
	streamflow_in (7)= Boles
	streamflow_in (8)= Garrick
	streamflow_in (9)= Lake_shastina
	streamflow_in (10)= Juniper_flat
	streamflow_in (11)= Whaleback_sheep
	streamflow_in (12)= Whitney
	streamflow_in (13)= Upper_willow
	streamflow_in (14)= Parks
	streamflow_in (15)= Eddy
	streamflow_in (16)= Lower_willow
	streamflow_in (17)= Upper_little_shasta
	streamflow_in (18)= Davis_gulch
	streamflow_in (19)= Middle_little_shasta
	streamflow_in (20)= Lower_Little_shasta
	streamflow_in (21)= Grass_lake
	streamflow_in (22)= Spring_creek_hart
	streamflow_in (23)= North_gate
    streamflow_in = streamflow_in * numdays
    
  end subroutine read_streamflow
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine SFR_streamflow(numdays, imonth)
    INTEGER :: numdays, imonth 
    integer :: i
    
    SFR_Flows = 0.            ! Reset all SFR flows to zero
    streamflow_out(:) = streamflow_in(:) - sw_irr(:)
    do i = 1, 23 ! The streamflow segment should be 33 
        SFR_Flows(i) = streamflow_out(i) / numdays
    end do	
    !if (imonth == 0 .or. imonth == 1 .or. imonth == 2 .or. imonth == 3 .or. imonth == 4 &
      !.or. imonth == 5 .or. imonth == 6 .or. imonth == 11) then ! Ditches only active from April - July        
      !SFR_Flows(31) = 0.
      !SFR_Flows(32) = 0.
    !else
      !SFR_Flows(31) = 8.  * 2446.58                                 ! Farmers Ditch Diversion (~8 cfs total diversion, leakage rate is about 6 cfs, assumed 2 cfs consumptive use)
      !SFR_Flows(32) = 16. * 2446.58                                 ! SVID Diversion (~16 cfs total diversion, leakage rate is about 14 cfs, assumed 2 cfs consumptive use)     	
    !end if
  end subroutine SFR_streamflow
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
end module


