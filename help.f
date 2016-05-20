!----------------------------------------------------------------------------
! Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
subroutine help

write(*,*) ' Usage: ./sig_bcz_d2 [options] < freqs'
write(*,*) ' '
write(*,*) ' Options:'
write(*,*) '   -h, --help        ', 'Show this help message and exit'
write(*,*) '   -v,--verbose      ', 'Prints information during code execution'
write(*,*) '   -p,--plots        ', 'Shows plots at the end of execution with the results'
write(*,*) ' '
write(*,*) ' File freqs has names of frequency files and stop at the end'
write(*,*) ' Frequency files have the structure:    l   n   w   error'

stop

end subroutine help