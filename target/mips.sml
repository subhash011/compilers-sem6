(*

AST of MIPS on SPIM

Short forms used:

| 1. Reg - Register | 8. W - Word          | 15. Ge - Greater than or Equal |
| 2. I - Immediate  | 9. HW - Half Word    | 16. Gt - Greater than          |
| 3. U - Unsigned   | 10. DW - Double Word | 17. Le - Less than or Equal    |
| 4. Q - Quotient   | 11. HI - hi register | 18. Lt - Less than             |
| 5. L - Left       | 12. LO - lo register | 19. Ne - Not equals            |
| 6. R - Right      | 13. Z - Zero         | 20. Addr - Address             |
| 7. B - Byte       | 14. Eq - Equals      | 21. UA - Unaligned             |

Convention:
- srci for ith source and dest for destination
- if they type is 'l then we are dealing with memory address and if the
type is 't then we are dealing with registers.

*)

structure MIPS =
struct

    type Immediate = int

    datatype Label = LUser of string
                | LTemp of int

    (* Registers 'at' and  'k' have been ignore since they are registered
    for the assembler and the OS respectively. *)
    datatype Regs = Zero
            | v of int
            | a of int
            | t of int
            | s of int
            | gp
            | sp
            | fp
            | ra

    datatype ('l, 't) ALU = Abs of {dest: 't, src1: 't}
                        | Add of {dest: 't, src1: 't, src2: 't}
                        | Add_I of {dest: 't, src1: 't, imm: Immediate}
                        | Add_U of {dest: 't, src1: 't, src2: 't}
                        | Add_I_U of {dest: 't, src1: 't, imm: Immediate}
                        | And of {dest: 't, src1: 't, src2: 't}
                        | And_I of {dest: 't, src1: 't, imm: Immediate}
                        | Div of {src1: 't, src2: 't}
                        | Div_U of {src1: 't, src2: 't}
                        | Div_Q of {dest: 't, src1: 't, src2: 't}
                        | Div_Q_U of {dest: 't, src1: 't, src2: 't}
                        | Mul of {dest: 't, src1: 't, src2: 't}
                        | Mul_O of {dest: 't, src1: 't, src2: 't}
                        | Mul_O_U of {dest: 't, src1: 't, src2: 't}
                        | Mult of {src1: 't, src2: 't}
                        | Mult_U of {src1: 't, src2: 't}
                        | Neg of {dest: 't, src1: 't}
                        | Neg_U of {dest: 't, src1: 't}
                        | Nor of {dest: 't, src1: 't, src2: 't}
                        | Not of {dest: 't, src1: 't}
                        | Or of {dest: 't, src1: 't, src2: 't}
                        | Or_I of {dest: 't, src1: 't, imm: Immediate}
                        | Rem of {dest: 't, src1: 't, src2: 't}
                        | Rem_U of {dest: 't, src1: 't, src2: 't}
                        | Rot_L of {dest: 't, src1: 't, src2: 't}
                        | Rot_R of {dest: 't, src1: 't, src2: 't}
                        | SLL of {dest: 't, src1: 't, imm: Immediate}
                        | SLLV of {dest: 't, src1: 't, src2: 't}
                        | SRA of {dest: 't, src1: 't, imm: Immediate}
                        | SRAV of {dest: 't, src1: 't, src2: 't}
                        | SRL of {dest: 't, src1: 't, imm: Immediate}
                        | SRLV of {dest: 't, src1: 't, src2: 't}
                        | Sub of {dest: 't, src1: 't, src2: 't}
                        | Sub_U of {dest: 't, src1: 't, src2: 't}
                        | XOR of {dest: 't, src1: 't, src2: 't}
                        | XOR_I of {dest: 't, src1: 't, imm: Immediate}

    and ('l, 't) ConstantMapping = LoadUpper_I of {dest: 't, imm: Immediate}
                                | Load_I of {dest: 't, imm: Immediate}

    and ('l, 't) LoadAndStore = Load_Addr of {dest: 't, src1: 'l}
                                | Load_B of {dest: 't, src1: 'l}
                                | Load_U_B of {dest: 't, src1: 'l}
                                | Load_DW of {dest: 't, src1: 'l}
                                | Load_HW of {dest: 't, src1: 'l}
                                | Load_U_HW of {dest: 't, src1: 'l}
                                | Load_W of {dest: 't, src1: 'l}
                                | Load_W_L of {dest: 't, src1: 'l}
                                | Load_W_R of {dest: 't, src1: 'l}
                                | UA_Load_HW of {dest: 't, src1: 'l}
                                | UA_Load_HW_U of {dest: 't, src1: 'l}
                                | UA_Load_W of {dest: 't, src1: 'l}
                                | Store_B of {src1: 't, dest: 'l}
                                | Store_DW of {src1: 't, dest: 'l}
                                | Store_HW of {src1: 't, dest: 'l}
                                | Store_W of {src1: 't, dest: 'l}
                                | Store_W_L of {src1: 't, dest: 'l}
                                | Store_W_R of {src1: 't, dest: 'l}
                                | UA_Store_HW of {src1: 't, dest: 'l}
                                | UA_Store_W of {src1: 't, dest: 'l}

    and ('l, 't) DataMovement = Move of {dest: 't, src1: 't}
                            | MoveFrom_HI of {dest: 't}
                            | MoveFrom_LO of {dest: 't}
                            | MoveTo_HI of {src1: 't}
                            | MoveTo_LO of {src1: 't}

    and ('l, 't) BranchAndJump = Branch of {dest: 'l}
                            | Branch_Eq of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Eq_Z of {dest: 'l, src1: 't}
                            | Branch_Ge of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Ge_U of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Ge_Z of {dest: 'l, src1: 't}
                            | Branch_Gt of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Gt_U of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Gt_Z of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Le of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Le_U of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Le_Z of {dest: 'l, src1: 't}
                            | Branch_Ge_Z_Ln of {dest: 'l, src1: 't}
                            | Branch_Lt_Z_Ln of {dest: 'l, src1: 't}
                            | Branch_Lt of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Lt_U of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Lt_Z of {dest: 'l, src1: 't}
                            | Branch_Ne of {dest: 'l, src1: 't, src2: 't}
                            | Branch_Ne_Z of {dest: 'l, src1: 't}
                            | Jump of {dest: 'l}
                            | Jump_Ln of {dest: 'l}
                            | Jump_Ln_Reg of {dest: 't}
                            | Jump_Reg of {dest: 't}

    and ('l, 't) Comparision = Set_Eq of {dest: 't, src1: 't, src2: 't}
                            | Set_Ge of {dest: 't, src1: 't, src2: 't}
                            | Set_Ge_U of {dest: 't, src1: 't, src2: 't}
                            | Set_Gt of {dest: 't, src1: 't, src2: 't}
                            | Set_Gt_U of {dest: 't, src1: 't, src2: 't}
                            | Set_Le of {dest: 't, src1: 't, src2: 't}
                            | Set_Le_U of {dest: 't, src1: 't, src2: 't}
                            | Set_Lt of {dest: 't, src1: 't, src2: 't}
                            | Set_Lt_I of {dest: 't, src1: 't, imm: Immediate}
                            | Set_Lt_U of {dest: 't, src1: 't, src2: 't}
                            | Set_Lt_I_U of {dest: 't, src1: 't, imm: Immediate}
                            | Set_Ne of {dest: 't, src1: 't, src2: 't}

    and ('l, 't) ExceptionTrap = RetFromException
                            | Syscall
                            | Break
                            | NoOp

end