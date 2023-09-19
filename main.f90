program main
  implicit none

  ! Ignore gramm. errors please

  ! This poetry comes without any warranty
  ! any physical or mental health issues
  ! caused by its low quality is not on 
  ! the resposibitlity of the author(s)

  ! Key and alphabet are now globals
  integer*4 :: key = 1
  character(len=61) :: alphabet
  character(len=100) :: message

  interface operator(+)
    procedure decipher_add_string
  end interface operator(+)
  
  call load_alphabet()

  call print_riddle()

  print *, "Enter your answer to the riddle:"
  read *, key

  print *, "Enter the text you want to be decrypted:"
  read *, message
  call decrypt(message)

  print*, "Ot's answer is: ", trim(message)

contains

  function decipher_add_string(operand1, operand2) result(res)
    implicit none

    character(len=*), intent(in) :: operand1, operand2
    character(len=len(operand2)) :: tmp
    character(len=len_trim(operand1) + len(operand2) + 1) :: res

    tmp = operand2
    call decrypt(tmp)
    res = trim(operand1) // " " // trim(tmp)

  end function decipher_add_string

  subroutine load_alphabet()
    implicit none
    integer*4 :: ierr
    character(len=380) :: multiline

    open(unit=10, file="alphabet.txt", status='old', action='read', iostat=ierr)
    if (ierr /= 0) then
      multiline = "ERROR:" // new_line('a') // &
      "I want to make it loud and clear," // new_line('a') // &
      "an alphabet is missing here!" // new_line('a') // &
      "You might know it that's the fun" // new_line('a') // &
      "a silly string of 61!" // new_line('a') // &
      "But where to find it you may ask," // new_line('a') // &
      "that's the riddle that's the task!" // new_line('a')

      ! Let's ask AD what these words mean
      ! Let's ask AD what her tongue would have seen
      ! Just decipher the word Vaqe3
      ! Just decipher the word dXGzl24

      multiline = trim(multiline) // &
      "But don't worry I am never mean," // new_line('a') // &
      "help is in the words that cannot be seen!" // new_line('a')

      write (0,*) trim(multiline)
      stop 0
    end if

    read (10, "(A61)") alphabet

    close(10)

  end subroutine load_alphabet

  subroutine print_riddle()
    implicit none
    character(len=260) :: multiline

    multiline = "RIDDLE:" // new_line('a') // &
    "Through mountains high and valleys low," // new_line('a') // &
    "A journey" + "f1Vmeln" // ", a story to sow." // new_line('a') // &
    "It is not" + "W4omN6r16" // " if the" + "oer1n" // " go well," // new_line('a') // &
    "or if everything fails to the gates of hell." // new_line('a') // &
    "Then" + "6Gz" // " only things that" + "hmf16" // " and" + "4r66zN" // "," // new_line('a') // &
    "is" + "Gmx" + "4r1K" + "4Wezn" + "6Gz" + "hNzx" + "Grl" + "Nf1" + "6msz6GzN" // "?" // new_line('a')

    ! According to previous riddle, SC does not count as a part of the crew
    ! Round down

    print *, multiline
    
  end subroutine print_riddle

  subroutine decrypt(message)
    implicit none
    character(len=*), intent(inout) :: message

    integer*4 :: i, y, position, resultIndex
    character(len=100) :: encryptedValue
    character(1) :: currentChar, decodedChar
    

    do i = 1, len_trim(message)
      currentChar = message(i:i)

      do y = 1, len(alphabet)
        if (alphabet(y:y) == currentChar) then
          position = y-key
          ! Checking overflow
          position = position - len(alphabet) * &
                                floor(1.0_4 * (position - 1_4) / len(alphabet))
          exit
        end if
      end do

      decodedChar = alphabet(position:position)
      write(message(i:i), '(A, 2X, A)') decodedChar
    end do

  end subroutine decrypt
end program main
