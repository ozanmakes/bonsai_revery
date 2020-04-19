(** Keyboard keys *)

type t =
  | Unknown
  | Backspace
  | Tab
  | Return
  | Escape
  | Space
  | Exclaim
  | Quotedbl
  | Hash
  | Dollar
  | Percent
  | Ampersand
  | Quote
  | Leftparen
  | Rightparen
  | Asterisk
  | Plus
  | Comma
  | Minus
  | Period
  | Slash
  | Num_0
  | Num_1
  | Num_2
  | Num_3
  | Num_4
  | Num_5
  | Num_6
  | Num_7
  | Num_8
  | Num_9
  | Colon
  | Semicolon
  | Less
  | Equals
  | Greater
  | Question
  | At
  | Leftbracket
  | Backslash
  | Rightbracket
  | Caret
  | Underscore
  | Backquote
  | A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Delete
  | Capslock
  | F1
  | F2
  | F3
  | F4
  | F5
  | F6
  | F7
  | F8
  | F9
  | F10
  | F11
  | F12
  | Printscreen
  | Scrolllock
  | Pause
  | Insert
  | Home
  | Pageup
  | End
  | Pagedown
  | Right
  | Left
  | Down
  | Up
  | Numlockclear
  | Kp_DIVIDE
  | Kp_MULTIPLY
  | Kp_MINUS
  | Kp_PLUS
  | Kp_ENTER
  | Kp_1
  | Kp_2
  | Kp_3
  | Kp_4
  | Kp_5
  | Kp_6
  | Kp_7
  | Kp_8
  | Kp_9
  | Kp_0
  | Kp_PERIOD
  | Application
  | Power
  | Kp_EQUALS
  | F13
  | F14
  | F15
  | F16
  | F17
  | F18
  | F19
  | F20
  | F21
  | F22
  | F23
  | F24
  | Execute
  | Help
  | Menu
  | Select
  | Stop
  | Again
  | Undo
  | Cut
  | Copy
  | Paste
  | Find
  | Mute
  | Volumeup
  | Volumedown
  | Kp_COMMA
  | Kp_EQUALSAS400
  | Alterase
  | Sysreq
  | Cancel
  | Clear
  | Prior
  | Return2
  | Separator
  | Out
  | Oper
  | Clearagain
  | Crsel
  | Exsel
  | Kp_00
  | Kp_000
  | Thousandsseparator
  | Decimalseparator
  | Currencyunit
  | Currencysubunit
  | Kp_LEFTPAREN
  | Kp_RIGHTPAREN
  | Kp_LEFTBRACE
  | Kp_RIGHTBRACE
  | Kp_TAB
  | Kp_BACKSPACE
  | Kp_A
  | Kp_B
  | Kp_C
  | Kp_D
  | Kp_E
  | Kp_F
  | Kp_XOR
  | Kp_POWER
  | Kp_PERCENT
  | Kp_LESS
  | Kp_GREATER
  | Kp_AMPERSAND
  | Kp_DBLAMPERSAND
  | Kp_VERTICALBAR
  | Kp_DBLVERTICALBAR
  | Kp_COLON
  | Kp_HASH
  | Kp_SPACE
  | Kp_AT
  | Kp_EXCLAM
  | Kp_MEMSTORE
  | Kp_MEMRECALL
  | Kp_MEMCLEAR
  | Kp_MEMADD
  | Kp_MEMSUBTRACT
  | Kp_MEMMULTIPLY
  | Kp_MEMDIVIDE
  | Kp_PLUSMINUS
  | Kp_CLEAR
  | Kp_CLEARENTRY
  | Kp_BINARY
  | Kp_OCTAL
  | Kp_DECIMAL
  | Kp_HEXADECIMAL
  | Lctrl
  | Lshift
  | Lalt
  | Lgui
  | Rctrl
  | Rshift
  | Ralt
  | Rgui
  | Mode
  | Audionext
  | Audioprev
  | Audiostop
  | Audioplay
  | Audiomute
  | Mediaselect
  | Www
  | Mail
  | Calculator
  | Computer
  | Ac_SEARCH
  | Ac_HOME
  | Ac_BACK
  | Ac_FORWARD
  | Ac_STOP
  | Ac_REFRESH
  | Ac_BOOKMARKS
  | Brightnessdown
  | Brightnessup
  | Displayswitch
  | Kbdillumtoggle
  | Kbdillumdown
  | Kbdillumup
  | Eject
  | Sleep
[@@deriving equal, sexp]

let of_keycode = function
  | 8 -> Backspace
  | 9 -> Tab
  | 13 -> Return
  | 27 -> Escape
  | 32 -> Space
  | 33 -> Exclaim
  | 34 -> Quotedbl
  | 35 -> Hash
  | 36 -> Dollar
  | 37 -> Percent
  | 38 -> Ampersand
  | 39 -> Quote
  | 40 -> Leftparen
  | 41 -> Rightparen
  | 42 -> Asterisk
  | 43 -> Plus
  | 44 -> Comma
  | 45 -> Minus
  | 46 -> Period
  | 47 -> Slash
  | 48 -> Num_0
  | 49 -> Num_1
  | 50 -> Num_2
  | 51 -> Num_3
  | 52 -> Num_4
  | 53 -> Num_5
  | 54 -> Num_6
  | 55 -> Num_7
  | 56 -> Num_8
  | 57 -> Num_9
  | 58 -> Colon
  | 59 -> Semicolon
  | 60 -> Less
  | 61 -> Equals
  | 62 -> Greater
  | 63 -> Question
  | 64 -> At
  | 91 -> Leftbracket
  | 92 -> Backslash
  | 93 -> Rightbracket
  | 94 -> Caret
  | 95 -> Underscore
  | 96 -> Backquote
  | 97 -> A
  | 98 -> B
  | 99 -> C
  | 100 -> D
  | 101 -> E
  | 102 -> F
  | 103 -> G
  | 104 -> H
  | 105 -> I
  | 106 -> J
  | 107 -> K
  | 108 -> L
  | 109 -> M
  | 110 -> N
  | 111 -> O
  | 112 -> P
  | 113 -> Q
  | 114 -> R
  | 115 -> S
  | 116 -> T
  | 117 -> U
  | 118 -> V
  | 119 -> W
  | 120 -> X
  | 121 -> Y
  | 122 -> Z
  | 127 -> Delete
  | 1073741881 -> Capslock
  | 1073741882 -> F1
  | 1073741883 -> F2
  | 1073741884 -> F3
  | 1073741885 -> F4
  | 1073741886 -> F5
  | 1073741887 -> F6
  | 1073741888 -> F7
  | 1073741889 -> F8
  | 1073741890 -> F9
  | 1073741891 -> F10
  | 1073741892 -> F11
  | 1073741893 -> F12
  | 1073741894 -> Printscreen
  | 1073741895 -> Scrolllock
  | 1073741896 -> Pause
  | 1073741897 -> Insert
  | 1073741898 -> Home
  | 1073741899 -> Pageup
  | 1073741901 -> End
  | 1073741902 -> Pagedown
  | 1073741903 -> Right
  | 1073741904 -> Left
  | 1073741905 -> Down
  | 1073741906 -> Up
  | 1073741907 -> Numlockclear
  | 1073741908 -> Kp_DIVIDE
  | 1073741909 -> Kp_MULTIPLY
  | 1073741910 -> Kp_MINUS
  | 1073741911 -> Kp_PLUS
  | 1073741912 -> Kp_ENTER
  | 1073741913 -> Kp_1
  | 1073741914 -> Kp_2
  | 1073741915 -> Kp_3
  | 1073741916 -> Kp_4
  | 1073741917 -> Kp_5
  | 1073741918 -> Kp_6
  | 1073741919 -> Kp_7
  | 1073741920 -> Kp_8
  | 1073741921 -> Kp_9
  | 1073741922 -> Kp_0
  | 1073741923 -> Kp_PERIOD
  | 1073741925 -> Application
  | 1073741926 -> Power
  | 1073741927 -> Kp_EQUALS
  | 1073741928 -> F13
  | 1073741929 -> F14
  | 1073741930 -> F15
  | 1073741931 -> F16
  | 1073741932 -> F17
  | 1073741933 -> F18
  | 1073741934 -> F19
  | 1073741935 -> F20
  | 1073741936 -> F21
  | 1073741937 -> F22
  | 1073741938 -> F23
  | 1073741939 -> F24
  | 1073741940 -> Execute
  | 1073741941 -> Help
  | 1073741942 -> Menu
  | 1073741943 -> Select
  | 1073741944 -> Stop
  | 1073741945 -> Again
  | 1073741946 -> Undo
  | 1073741947 -> Cut
  | 1073741948 -> Copy
  | 1073741949 -> Paste
  | 1073741950 -> Find
  | 1073741951 -> Mute
  | 1073741952 -> Volumeup
  | 1073741953 -> Volumedown
  | 1073741957 -> Kp_COMMA
  | 1073741958 -> Kp_EQUALSAS400
  | 1073741977 -> Alterase
  | 1073741978 -> Sysreq
  | 1073741979 -> Cancel
  | 1073741980 -> Clear
  | 1073741981 -> Prior
  | 1073741982 -> Return2
  | 1073741983 -> Separator
  | 1073741984 -> Out
  | 1073741985 -> Oper
  | 1073741986 -> Clearagain
  | 1073741987 -> Crsel
  | 1073741988 -> Exsel
  | 1073742000 -> Kp_00
  | 1073742001 -> Kp_000
  | 1073742002 -> Thousandsseparator
  | 1073742003 -> Decimalseparator
  | 1073742004 -> Currencyunit
  | 1073742005 -> Currencysubunit
  | 1073742006 -> Kp_LEFTPAREN
  | 1073742007 -> Kp_RIGHTPAREN
  | 1073742008 -> Kp_LEFTBRACE
  | 1073742009 -> Kp_RIGHTBRACE
  | 1073742010 -> Kp_TAB
  | 1073742011 -> Kp_BACKSPACE
  | 1073742012 -> Kp_A
  | 1073742013 -> Kp_B
  | 1073742014 -> Kp_C
  | 1073742015 -> Kp_D
  | 1073742016 -> Kp_E
  | 1073742017 -> Kp_F
  | 1073742018 -> Kp_XOR
  | 1073742019 -> Kp_POWER
  | 1073742020 -> Kp_PERCENT
  | 1073742021 -> Kp_LESS
  | 1073742022 -> Kp_GREATER
  | 1073742023 -> Kp_AMPERSAND
  | 1073742024 -> Kp_DBLAMPERSAND
  | 1073742025 -> Kp_VERTICALBAR
  | 1073742026 -> Kp_DBLVERTICALBAR
  | 1073742027 -> Kp_COLON
  | 1073742028 -> Kp_HASH
  | 1073742029 -> Kp_SPACE
  | 1073742030 -> Kp_AT
  | 1073742031 -> Kp_EXCLAM
  | 1073742032 -> Kp_MEMSTORE
  | 1073742033 -> Kp_MEMRECALL
  | 1073742034 -> Kp_MEMCLEAR
  | 1073742035 -> Kp_MEMADD
  | 1073742036 -> Kp_MEMSUBTRACT
  | 1073742037 -> Kp_MEMMULTIPLY
  | 1073742038 -> Kp_MEMDIVIDE
  | 1073742039 -> Kp_PLUSMINUS
  | 1073742040 -> Kp_CLEAR
  | 1073742041 -> Kp_CLEARENTRY
  | 1073742042 -> Kp_BINARY
  | 1073742043 -> Kp_OCTAL
  | 1073742044 -> Kp_DECIMAL
  | 1073742045 -> Kp_HEXADECIMAL
  | 1073742048 -> Lctrl
  | 1073742049 -> Lshift
  | 1073742050 -> Lalt
  | 1073742051 -> Lgui
  | 1073742052 -> Rctrl
  | 1073742053 -> Rshift
  | 1073742054 -> Ralt
  | 1073742055 -> Rgui
  | 1073742081 -> Mode
  | 1073742082 -> Audionext
  | 1073742083 -> Audioprev
  | 1073742084 -> Audiostop
  | 1073742085 -> Audioplay
  | 1073742086 -> Audiomute
  | 1073742087 -> Mediaselect
  | 1073742088 -> Www
  | 1073742089 -> Mail
  | 1073742090 -> Calculator
  | 1073742091 -> Computer
  | 1073742092 -> Ac_SEARCH
  | 1073742093 -> Ac_HOME
  | 1073742094 -> Ac_BACK
  | 1073742095 -> Ac_FORWARD
  | 1073742096 -> Ac_STOP
  | 1073742097 -> Ac_REFRESH
  | 1073742098 -> Ac_BOOKMARKS
  | 1073742099 -> Brightnessdown
  | 1073742100 -> Brightnessup
  | 1073742101 -> Displayswitch
  | 1073742102 -> Kbdillumtoggle
  | 1073742103 -> Kbdillumdown
  | 1073742104 -> Kbdillumup
  | 1073742105 -> Eject
  | 1073742106 -> Sleep
  | _ -> Unknown
