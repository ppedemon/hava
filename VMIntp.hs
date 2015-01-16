module VMIntp(vmintp) where

import Array

import VMMonad
import {-# SOURCE #-} VM

import VMOps
import WideOps
import ArithOps
import ArrayOps
import BasicOps
import BranchOps


{--------------------------------------------------------------------
  VM's execution engine intepreter. A huge case branching among
  all the Java defined opcodes. The opcode is fetched from the
  given code array, using the given PC value.
  
  This function modifies the VM residing in the monad's state in
  two ways:
  
    - first, it executes the effects of the selected intepreter 
      function
    - then, it updates the VM's PC value applying the PC function 
      (pf) returned by the previously executed intepreter function
--------------------------------------------------------------------}

vmintp :: Int ->  Array Int Int -> VM_ ()
vmintp pc code = 
  let f = fetch (code ! pc)
  in do pf <- f pc code
        vm <- getS
        setS (vmsetPC vm pf)
        
fetch :: Int -> Int -> Array Int Int -> VM_ (Int -> Int)
fetch op = 
  case op of
    0   -> nop
    1   -> vmlift aconst_null
    2   -> vmlift (iconst op)
    3   -> vmlift (iconst op)
    4   -> vmlift (iconst op)
    5   -> vmlift (iconst op)
    6   -> vmlift (iconst op)
    7   -> vmlift (iconst op)
    8   -> vmlift (iconst op)
    9   -> vmlift (lconst op)
    10  -> vmlift (lconst op)
    11  -> vmlift (fconst op)
    12  -> vmlift (fconst op)
    13  -> vmlift (fconst op)
    14  -> vmlift (dconst op)
    15  -> vmlift (dconst op)
    16  -> vmlift bipush
    17  -> vmlift sipush
    18  -> ldc
    19  -> ldc_w
    20  -> ldc2_w
    21  -> vmlift iload
    22  -> vmlift lload
    23  -> vmlift fload
    24  -> vmlift dload
    25  -> vmlift aload
    26  -> vmlift (iloadn op)
    27  -> vmlift (iloadn op)
    28  -> vmlift (iloadn op)
    29  -> vmlift (iloadn op)
    30  -> vmlift (lloadn op)
    31  -> vmlift (lloadn op)
    32  -> vmlift (lloadn op)
    33  -> vmlift (lloadn op)
    34  -> vmlift (floadn op)
    35  -> vmlift (floadn op)
    36  -> vmlift (floadn op)
    37  -> vmlift (floadn op)
    38  -> vmlift (dloadn op)
    39  -> vmlift (dloadn op)
    40  -> vmlift (dloadn op)
    41  -> vmlift (dloadn op)
    42  -> vmlift (aloadn op)
    43  -> vmlift (aloadn op)
    44  -> vmlift (aloadn op)
    45  -> vmlift (aloadn op)
    46  -> iaload
    47  -> laload
    48  -> faload
    49  -> daload
    50  -> aaload
    51  -> baload
    52  -> caload
    53  -> saload
    54  -> vmlift (istore)
    55  -> vmlift (lstore)
    56  -> vmlift (fstore)
    57  -> vmlift (dstore)
    58  -> vmlift (astore)
    59  -> vmlift (istoren op)
    60  -> vmlift (istoren op)
    61  -> vmlift (istoren op)
    62  -> vmlift (istoren op)
    63  -> vmlift (lstoren op)
    64  -> vmlift (lstoren op)
    65  -> vmlift (lstoren op)
    66  -> vmlift (lstoren op)
    67  -> vmlift (fstoren op)
    68  -> vmlift (fstoren op)
    69  -> vmlift (fstoren op)
    70  -> vmlift (fstoren op)
    71  -> vmlift (dstoren op)
    72  -> vmlift (dstoren op)
    73  -> vmlift (dstoren op)
    74  -> vmlift (dstoren op)
    75  -> vmlift (astoren op)
    76  -> vmlift (astoren op)
    77  -> vmlift (astoren op)
    78  -> vmlift (astoren op)
    79  -> iastore
    80  -> lastore
    81  -> fastore
    82  -> dastore
    83  -> aastore
    84  -> bastore
    85  -> castore
    86  -> sastore
    87  -> vmlift pop
    88  -> vmlift pop2
    89  -> vmlift dup
    90  -> vmlift dup_x1
    91  -> vmlift dup_x2
    92  -> vmlift dup2
    93  -> vmlift dup2_x1
    94  -> vmlift dup2_x2
    95  -> vmlift swap
    96  -> vmlift iadd
    97  -> vmlift ladd
    98  -> vmlift fadd
    99  -> vmlift dadd
    100 -> vmlift isub
    101 -> vmlift lsub
    102 -> vmlift fsub
    103 -> vmlift dsub
    104 -> vmlift imul
    105 -> vmlift lmul
    106 -> vmlift fmul
    107 -> vmlift dmul    
    108 -> idiv
    109 -> ldiv
    110 -> vmlift fdiv
    111 -> vmlift ddiv
    112 -> vmlift irem
    113 -> vmlift lrem
    114 -> vmlift frem
    115 -> vmlift drem
    116 -> vmlift ineg
    117 -> vmlift lneg
    118 -> vmlift fneg
    119 -> vmlift dneg
    120 -> vmlift ishl
    121 -> vmlift lshl
    122 -> vmlift ishr
    123 -> vmlift lshr
    124 -> vmlift iushr
    125 -> vmlift lushr
    126 -> vmlift iand
    127 -> vmlift land
    128 -> vmlift ior
    129 -> vmlift lor
    130 -> vmlift ixor
    131 -> vmlift lxor
    132 -> vmlift iinc
    133 -> vmlift i2l
    134 -> vmlift i2f
    135 -> vmlift i2d
    136 -> vmlift l2i
    137 -> vmlift l2f
    138 -> vmlift l2d
    139 -> vmlift f2i
    140 -> vmlift f2l
    141 -> vmlift f2d
    142 -> vmlift d2i
    143 -> vmlift d2l
    144 -> vmlift d2f
    145 -> vmlift i2b
    146 -> vmlift i2c
    147 -> vmlift i2s
    148 -> vmlift lcmp
    149 -> vmlift fcmpl
    150 -> vmlift fcmpg
    151 -> vmlift dcmpl
    152 -> vmlift dcmpg
    153 -> vmlift ifeq
    154 -> vmlift ifne
    155 -> vmlift iflt
    156 -> vmlift ifge
    157 -> vmlift ifgt
    158 -> vmlift ifle
    159 -> vmlift if_icmpeq
    160 -> vmlift if_icmpne
    161 -> vmlift if_icmplt
    162 -> vmlift if_icmpge
    163 -> vmlift if_icmpgt
    164 -> vmlift if_icmple
    165 -> vmlift if_acmpeq
    166 -> vmlift if_acmpne
    167 -> vmlift goto
    168 -> vmlift jsr
    169 -> vmlift ret
    170 -> vmlift tableswitch
    171 -> vmlift lookupswitch
    172 -> ireturn
    173 -> lreturn
    174 -> freturn
    175 -> dreturn
    176 -> areturn
    177 -> vreturn
    178 -> getstatic
    179 -> putstatic
    180 -> getfield
    181 -> putfield
    182 -> invokevirtual
    183 -> invokespecial
    184 -> invokestatic
    185 -> invokeinterface
    186 -> error $ show "VM error - bytecode 186 should be unused!"
    187 -> new
    188 -> newarray
    189 -> anewarray
    190 -> arraylength
    191 -> athrow
    192 -> checkcast
    193 -> instanceof
    194 -> unimp op   -- We don't provide support for aquiring locks
    195 -> unimp op   -- We don't provide support for releasing locks
    196 -> vmlift wide
    197 -> multianewarray
    198 -> vmlift ifnull
    199 -> vmlift ifnonnull
    200 -> vmlift goto_w
    201 -> vmlift jsr_w
    _   -> unimp op


unimp :: Int -> Int -> Array Int Int -> VM_ (Int -> Int)
unimp op pc code = 
  do vm <- getS
     error $ "fetch: Unimplemented bytecode: "   ++ show op ++ 
             "\nStack: " ++ show (vmgetStack vm) ++
             "\nPC   : " ++ show pc              ++
             "\nCode : " ++ show code
