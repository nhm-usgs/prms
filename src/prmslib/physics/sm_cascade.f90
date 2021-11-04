submodule (PRMS_CASCADE) sm_cascade
  contains
    module subroutine init_Cascade(this, ctl_data, model_basin, model_gw, model_summary)
      implicit none

      class(Cascade), intent(inout) :: this
        !! Cascade class
      type(Control), intent(in) :: ctl_data
        !! Control file parameters
      type(Basin), intent(in) :: model_basin
      type(Gwflow), intent(in) :: model_gw
      type(Summary), intent(inout) :: model_summary

      ! Local variables
      integer(i32) :: cgwr
      integer(i32) :: chru
      integer(i32) :: ii
      integer(i32) :: iret
      integer(i32) :: itest
      integer(i32) :: jj
      integer(i32) :: kk

      ! ---------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag, &
                cascadegw_flag => ctl_data%cascadegw_flag, &
                gwr_swale_flag => ctl_data%gwr_swale_flag, &
                param_hdl => ctl_data%param_hdl, &
                print_debug => ctl_data%print_debug, &

                active_gwrs => model_basin%active_gwrs, &
                active_hrus => model_basin%active_hrus, &
                gwr_type => model_basin%gwr_type, &
                hru_type => model_basine%hru_type, &
                nhru => model_basin%nhru, &
                gwr_route_order => model_basin%gwr_route_order, &
                hru_route_order => model_basin%hru_route_order, &

                ngw => model_gw%ngw)

        call this%set_module_info(name=MODNAME, desc=MODDESC, version=MODVERSION)

        if (print_debug > -2) then
          ! Output module and version information
          call this%print_module_info()
        endif

        ! Dimensions
        this%ncascade = param_hdl%get_dimension('ncascade')
        this%ncascdgw = param_hdl%get_dimension('ncascdgw')
        this%ndown = param_hdl%get_dimension('ndown')

        ! Parameters
        if (cascade_flag > 0) then
          allocate(this%ncascade_hru(nhru))

          allocate(hru_down_id(this%ncascade))
          allocate(hru_pct_up(this%ncascade))
          allocate(hru_strmseg_down_id(this%ncascade))
          allocate(hru_up_id(this%ncascade))
        end if

        ! WARNING: Why is this allowed when cascade_flag = 0?
        if (cascade_flag == 2) then
          this%cascade_tol = 5.0
          this%cascade_flg = 1
          this%circle_switch = 0
        else
          call param_hdl%get_variable('cascade_tol', this%cascade_tol)
          call param_hdl%get_variable('cascade_flg', this%cascade_flg)
          call param_hdl%get_variable('circle_switch', this%circle_switch)
        end if

        if (cascade_flag > 0) then
          call init_cascade_second(ctl_data, model_basin, itest)
        end if

        if (cascadegw_flag > 0) then
          allocate(this%ncascade_gwr(ngw))

          allocate(this%gw_up_id(this%ncascdgw))
          allocate(this%gw_strmseg_down_id(this%ncascdgw))
          allocate(this%gw_down_id(this%ncascdgw))
          allocate(this%gw_pct_up(this%ncascdgw))
        end if

        ! if (Print_debug == 13) call PRMS_open_module_file(MSGUNT, 'cascade.msgs')

        iret = 0
        if (cascadegw_flag > 0) then
          allocate(this%gwr_down(this%ndown, ngw))
          allocate(this%gwr_down_frac(this%ndown, ngw))
          allocate(this%cascade_gwr_area(this%ndown, ngw))

          if (cascadegw_flag == 1) then
            ! call initgw_cascade(iret)  (ln 221)
            ! if (iret == 1) ERROR STOP - 2
          else
            ! cascadegw_flag == 2 so GWR cascades are set to HRU cascades

            ! WARNING: Next three lines change variables owned by model_basin
            active_gwrs = active_hrus
            gwr_route_order = hru_route_order
            gwr_type = hru_type

            this%gwr_down = this%hru_down
            this%ncascade_gwr = this%ncascade_hru

            do ii = 1, ngw
              do jj = 1, this%ndown
                this%gwr_down_frac(jj, ii) = this%hru_down_frac(jj, ii)
                this%cascade_gwr_area(jj, ii) = this%cascade_area(jj, ii)
              end do
            end do
          end if

          if (gwr_swale_flag == 0) then
            do ii = 1, active_gwrs
              cgwr = gwr_route_order(ii)

              if (gwr_type(cgwr) == 3) then
                write(*, *) 'ERROR, GWR is a swale when gwr_swale_flag = 0, GWR:', cgwr
                iret = 1
              end if
            end do
          end if
        end if

        if (itest /= 0 .or. iret /= 0) then
          ERROR STOP - 2
        end if

        if (print_debug == 13) then
          if (cascade_flag > 0) then
            write(MSGUNT, 9001)

            kk = 0
            do ii = 1, active_hrus
              chru = hru_route_order(ii)

              do jj = 1, this%ncascade_hru(chru)
                kk = kk + 1
                write(MSGUNT, *) kk, chru, this%hru_down(jj, chru), this%hru_down_frac(jj, chru) * 100.0
              end do
            end do
          end if

          if (cascadegw_flag > 0) then
            write(MSGUNT, 9002)

            kk = 0
            do ii = 1, active_gwrs
              cgwr = gwr_route_order(ii)

              do jj = 1, this%ncascade_gwr(cgwr)
                kk = kk + 1
                write(MSGUNT, *) kk, cgwr, this%gwr_down_frac(jj, cgwr), this%gwr_down_frac(jj, cgwr) * 100.0
              end do
            end do
          end if

          close(MSGUNT)
        end if

        9001 format(//, 18X, 'UP HRU', 4X, 'doWN HRU    FRACTION')
        9002 format(//, 18X, 'UP GWR', 4X, 'doWN GWR    FRACTION')
      end associate
    end subroutine

    module subroutine cleanup_Cascade(this, ctl_data)
      implicit none
      class(Cascade) :: this
        !! Cascade class
      type(Control), intent(in) :: ctl_data

      ! ---------------------------------------------------------------------
      associate(cascade_flag => ctl_data%cascade_flag, &
                cascadegw_flag => ctl_data%cascadegw_flag)

        if (cascade_flag > 0) then
          deallocate(this%hru_down)
          deallocate(this%hru_down_frac)
          deallocate(this%hru_down_fracwt)
          deallocate(this%cascade_area)
        end if

        if (cascadegw_flag > 0) then
          deallocate(this%gwr_down)
          deallocate(this%gwr_down_frac)
          deallocate(this%cascade_gwr_area)
        end if
      end associate
    end subroutine

    subroutine init_cascade_second(this, ctl_data, model_basin, iret)
      implicit none

      class(Cascade), intent(inout) :: this
      type(Control), intent(in) :: ctl_data
      type(Basin), intent(in) :: model_basin
      integer(i32), intent(out) :: iret

      ! Local variables
      integer(i32) :: cgwr
      integer(i32) :: chru
      integer(i32) :: ii

      real(r32), allocatable :: hru_frac(:)


      associate(cascade_flag => ctl_data%cascade_flag, &
                cascadegw_flag => ctl_data%cascadegw_flag, &
                print_debug => ctl_data%print_debug, &

                active_hrus => model_basin%active_hrus, &
                hru_area => model_basin%hru_area, &
                hru_route_order => model_basin%hru_route_order, &
                hru_type => model_basin%hru_type, &
                nhru => model_basin%nhru, &
                nsegment => model_basin%nsegment)

        iret = 0

        ! Cascade parameters
        this%ndown = 1
        if (cascade_flag == 1) then
          call param_hdl%get_variable('hru_down_id', this%hru_down_id)
          call param_hdl%get_variable('hru_pct_up', this%hru_pct_up)
          call param_hdl%get_variable('hru_strmseg_down_id', this%hru_strmseg_down_id)
          call param_hdl%get_variable('hru_up_id', this%hru_up_id)

          ! Figure out the maximum number of cascade links from all HRUs, to set dimensions for 2D arrays
          this%ncascade_hru = 0
          do ii = 1, this%ncascade
            chru = this%hru_up_id(ii)

            if (chru > 0) then
              jdn = this%hru_down_id(ii)
              this%ncascade_hru(chru) = this%ncascade_hru(chru) + 1

              if (this%ncascade_hru(chru) > this%ndown) then
                this%ndown = this%ncascade_hru(chru)
              end if
            end if
          end do
        else if (cascade_flag == 2) then
          ! Simple 1-to-1 cascades, ncascade = nhru
          allocate(this%hru_segment(nhru))
          call param_hdl%get_variable('hru_segment', this%hru_segment)

          do ii = 1, nhru
            this%hru_up_id(ii) = ii
            this%hru_strmseg_down_id(ii) = this%hru_segment(ii)
          end do

          this%hru_down_id = 0
          this%hru_pct_up = 1.0
          deallocate(this%hru_segment)
        else
          write(*,*) 'ERROR - Unkown cascade_flag value, ', cascade_flag
          STOP
        end if

        if (cascadegw_flag == 1) then
          if (cascade_flag == 1) then
            call param_hdl%get_variable('gw_up_id', this%gw_up_id)
            call param_hdl%get_variable('gw_strmseg_down_id', this%gw_strmseg_down_id)
            call param_hdl%get_variable('gw_down_id', this%gw_down_id)
            call param_hdl%get_variable('gw_pct_up', this%gw_pct_up)

            this%ncascade_gwr = 0
            do ii = 1, this%ncascdgw
              cgwr = this%gw_up_id(ii)

              if (cgwr > 0) then
                jdn = this%gw_down_id(ii)
                this%ncascade_gwr(cgwr) = this%ncascade_gwr(cgwr) + 1

                if (this%ncascade_gwr(cgwr) > this%ndown) this%ndown = this%ncascade_gwr(cgwr)
              end if
            end do
          else if (cascade_flag == 2) then
            ! NOTE: So cascade_flag can override cascadegw_flag??
            ! DEBUG: ii would set to nhru here - is this correct?
            this%gw_up_id = this%hru_up_id(ii)
            this%gw_strmseg_down_id = this%hru_strmseg_down_id
            this%gw_down_id = 0
            this%gw_pct_up = 1.0
          end if
        end if

        ! ??rsr, why 15??
        if (this%ndown > 15 .and. print_debug == 13) write(MSGUNT, *) 'possible ndown issue', this%ndown

        ! Allocate HRU variables
        allocate(this%cascade_area(this%ndown, nhru))
        allocate(this%hru_down_frac(this%ndown, nhru))
        allocate(this%hru_down_fracwt(this%ndown, nhru))
        allocate(this%hru_down(this%ndown, nhru))
        allocate(hru_frac(nhru))

        hru_frac = 0.0
        this%cascade_area = 0.0
        this%hru_down = 0
        this%hru_down_frac = 0.0
        this%hru_down_fracwt = 0.0
        this%ncascade_hru = 0

        do ii = 1, this%ncascade
          kup = this%hru_up_id(ii)

          if (kup < 1) then
            write(*, *) 'Cascade ignored as hru_up_id < 1, cascade:', ii, ', hru_up_id:', kup
            cycle
          end if

          jdn = this%hru_down_id(ii)
          frac = this%hru_pct_up(ii)

          if (frac > 0.9998) frac = 1.0

          istrm = this%hru_strmseg_down_id(ii)

          if (frac < 0.00001) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as hru_pct_up=0.0', ii, kup, jdn, frac, istrm
            end if
          else if (istrm > nsegment) then
              if (print_debug_debug == 13) then
                write(MSGUNT, 9004) 'Cascade ignored as segment > nsegment', ii, jup, jdn, frac, istrm
              end if
          else if (kup < 1 .and. jdn == 0) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as up and down HRU = 0', ii, jup, jdn, frac, istrm
            end if
          else if (istrm == 0 .and. jdn == 0) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as down HRU and segment = 0', ii, jup, jdn, frac, istrm
            end if
          else if (hru_type(kup) == 0) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as up HRU is inactive', ii, jup, jdn, frac, istrm
            end if
          else if (hru_type(kup) == 3) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as up HRU is a swale', ii, jup, jdn, frac, istrm
            end if
          else if (hru_type(kup) == 2 .and. istrm < 1) then
            if (print_debug == 13) then
              write(MSGUNT, 9004) 'Cascade ignored as lake HRU cannot cascade to an HRU', ii, jup, jdn, frac, istrm
            end if
          else
            if (jdn > 0 .and. istrm < 1) then
              if (hru_type == 0) then
                if (print_debug == 13) then
                  write(MSGUNT, 9004) 'Cascade ignored as down HRU is inactive', ii, jup, jdn, frac, istrm
                end if

                cycle
              end if
            end if

            carea = frac * hru_area(kup)
            ! Get rid of small cascades, redistribute fractions
            if (carea < this%cascade_tol .and. frac < 0.075) then
              if (print_debug == 13) write(MSGUNT, 9005) i, kup, jdn, frac * 100.0, carea
            elseif (this%cascade_flg == 1) then
              if (frac > hru_frac(kup)) then
                hru_frac(kup) = frac
                this%ncascade_hru(kup) = 1
                this%hru_down_frac(1, kup) = frac

                if (istrm > 0) then
                  this%hru_down(1, kup) = -istrm
                else
                  this%hru_down(1, kup) = jdn
                end if
              end if
            else
              hru_frac(kup) = hru_frac(kup) + frac

              if (hru_frac(kup) > 1.0) then
                if (hru_frac(kup) > 1.00001) then
                  if (print_debug == 13) write (MSGUNT, 9004) 'Addition of cascade link makes contributing area add up to > 1.0, thus fraction reduced', ii, kup, jdn, hru_frac(kup), istrm
                end if

                frac = frac + 1.0 - hru_frac(kup)
                hru_frac(kup) = 1.0
              end if

              this%ncascade_hru(kup) = this%ncascade_hru(kup) + 1
              kk = this%ncascade_hru(kup)
              this%hru_down_frac(kk, kup) = frac

              if (istrm > 0) then
                this%hru_down(kk, kup) = -istrm
              else
                this%hru_down(kk, kup) = jdn
              end if
            end if
          end if
        end do

        ! How do we route headwater HRUs to a stream segment rather than
        ! across valleys**********************RSR???

        do ii = 1, active_hrus
          i = hru_route_order(ii)
          num = this%ncascade_hru(i)

          if (num == 0) CYCLE

          do k = 1, num
            frac = this%hru_down_frac(k, i)
            this%hru_down_frac(k, i) = frac + frac * (1.0 - hru_frac(i)) / hru_frac(i)
          end do

          k = 1
          do kk = 1, num
            dnhru = this%hru_down(kk, i)

            if (dnhru == 0) CYCLE

            this%hru_down_frac(k, i) = this%hru_down_frac(kk, i)
            this%hru_down(k, i) = dnhru
            j = num

            do while (j > kk)
              if (dnhru == this%hru_down(j, i)) then
                this%hru_down(j, i) = 0
                this%hru_down_frac(k, i) = this%hru_down_frac(k, i) + this%hru_down_frac(j, i)

                if (this%hru_down_frac(k, i) > 1.00001) then
                  if (print_debug == 13) then
                    write (MSGUNT, *) 'Combining cascade links makes contributing area add up to > 1.0, thus fraction reduced.'
                    write (MSGUNT, *) 'Up HRU:', i, ' Down HRU:', dnhru
                  end if

                  this%hru_down_frac(k, i) = 1.0
                end if

                if (dnhru < 0) then
                  ! Two cascades to same stream segment, combine
                  if (print_debug == 13) write (MSGUNT, 9002) i, 'stream segment', ABS(dnhru)
                else
                  ! Two cascades to same HRU, combine
                  if (print_debug == 13) write (MSGUNT, 9002) i, 'downslope HRU', dnhru
                end if

                this%ncascade_hru(i) = this%ncascade_hru(i) - 1
              end if

              j = j - 1
            end do

            this%cascade_area(k, i) = this%hru_down_frac(k, i) * hru_area(i)

            if (dnhru > 0) this%hru_down_fracwt(k, i) = this%cascade_area(k, i) / hru_area(dnhru)
            k = k + 1
          end do
        end do

        call order_hrus(iret)

        if (print_debug == 13) then
          write (MSGUNT, 9001)
          write (MSGUNT, 9003) (hru_route_order(i), i=1, iorder)
        end if

        deallocate (hru_frac)
        deallocate(this%hru_down_id)
        deallocate(this%hru_pct_up)
        deallocate(this%hru_strmseg_down_id)
        deallocate(this%hru_up_id)

        9001 format(/, 'HRU routing order:')
        9002 format('*** WARNING, combined multiple cascade paths from HRU:', I7, ' to ', A, ':', I7)
        9003 format(10I7)
        9004 format('*** WARNING, ', A, /, '    Cascade:', I7, '; up HRU:', &
                I7, '; down HRU:', I7, '; up fraction:', F8.4, '; stream segment:', I5)
        9005 format('*** WARNING, ignoring small cascade, carea<cascade_tol', &
                /, '    Cascade:', I7, '; HRU up:', I7, '; HRU down:', I7, &
                '; fraction up:', F8.2, '; cascade area:', F8.2)
      end associate
    end subroutine
end submodule


          ! if (cascadegw_flag == 1) then
          !   call param_hdl%get_variable('gw_up_id', this%gw_up_id)
          !   call param_hdl%get_variable('gw_strmseg_down_id', this%gw_strmseg_down_id)
          !   call param_hdl%get_variable('gw_down_id', this%gw_down_id)
          !   call param_hdl%get_variable('gw_pct_up', this%gw_pct_up)
          ! end if