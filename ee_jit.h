
#define SPECIAL_OP 0b000000 // Or ADDI too 
#define ADDIU_OP 0b001001 
#define ANDI_OP 0b001100
#define BEQ_OP 0b000100
#define BEQL_OP 0b010100
#define REGIMM_OP 0b000001
#define BGTZ_OP 0b000111
#define BGTZL_OP 0b010111
#define BLEZ_OP 0b000110
#define BLEZL_OP 0b010110
#define BNE_OP 0b000101
#define BNEL_OP 0b010101
#define DADDI_OP 0b011000
#define DADDIU_OP 0b011001
#define J_OP 0b000010
#define JAL_OP 0b000011
#define LB_OP 0b100000
#define LBU_OP 0b100100
#define LD_OP 0b110111
#define LDL_OP 0b011010
#define LDR_OP 0b011011
#define LH_OP 0b100001
#define LHU_OP 0b100101
#define LUI_OP 0b001111
#define LW_OP 0b100011
#define LWL_OP 0b100010
#define LWR_OP 0b100110
#define LWU_OP 0b100111
#define ORI_OP 0b001101
#define PREF_OP 0b110011
#define SB_OP 0b101000
#define SD_OP 0b111111
#define SDL_OP 0b101100
#define SDR_OP 0b101101
#define SH_OP 0b101001
#define SLTI_OP 0b001010
#define SLTIU_OP 0b001011
#define SW_OP 0b101011
#define SWL_OP 0b101010
#define SWR_OP 0b101110
#define XORI_OP 0b001110
#define MMI_OP 0b011100
#define LQ_OP 0b011110

#define ADD_FUNC 0b100000
#define ADDU_FUNC 0b100001
#define AND_FUNC 0b100100
#define BREAK_FUNC 0b001101
#define DADD_FUNC 0b101100
#define DADDU_FUNC 0b101101
#define DIV_FUNC 0b011010
#define DIVU_FUNC 0b011011
#define DSLL_FUNC 0b111000
#define DSLL32_FUNC 0b111100
#define DSLLV_FUNC 0b010100
#define DSRA_FUNC 0b111011
#define DSRA32_FUNC 0b111111
#define DSRAV_FUNC 0b010111
#define DSRL_FUNC 0b111010
#define DSRL32_FUNC 0b111110
#define DSRLV_FUNC 0b010110
#define DSUB_FUNC 0b101110
#define DSUBU_FUNC 0b101111
#define JALR_FUNC 0b001001
#define JR_FUNC 0b001000
#define MFHI_FUNC 0b010000
#define MFLO_FUNC 0b010010
#define MOVN_FUNC 0b001011
#define MOVZ_FUNC 0b001010
#define MTHI_FUNC 0b010001
#define MTLO_FUNC 0b010011
#define MULT_FUNC 0b011000
#define MULTU_FUNC 0b011001
#define NOR_FUNC 0b100111
#define OR_FUNC 0b100101
#define SLL_FUNC 0b000000
#define SLLV_FUNC 0b000100
#define SLT_FUNC 0b101010
#define SLTU_FUNC 0b101011
#define SRA_FUNC 0b000011
#define SRAV_FUNC 0b000111
#define SRL_FUNC 0b000010
#define SRLV_FUNC 0b000110
#define SUB_FUNC 0b100010
#define SUBU_FUNC 0b100011
#define SYNC_FUNC 0b001111
#define SYSCALL_FUNC 0b001100
#define TEQ_FUNC 0b110100
#define TGE_FUNC 0b110000
#define TGEU_FUNC 0b110001
#define TLT_FUNC 0b110010
#define TLTU_FUNC 0b110011
#define TNE_FUNC 0b110110
#define XOR_FUNC 0b100110
#define DIV1_FUNC 0b011010
#define DIVU1_FUNC 0b011011
#define MADD_FUNC 0b000000
#define MADD1_FUNC 0b100000
#define MADDU_FUNC 0b000001
#define MADDU1_FUNC 0b100001
#define MFHI1_FUNC 0b010000
#define MFLO1_FUNC 0b010010
#define MFSA_FUNC 0b101000
#define MTHI1_FUNC 0b010001
#define MTLO1_FUNC 0b010001
#define MTSA_FUNC 0b101001
#define MULT_FUNC 0b011000
#define MULT1_FUNC 0b011000
#define MULTU_FUNC 0b011001
#define MULTU1_FUNC 0b011001

// TODO: Add opcodes from PABSH 

// To be used with SYNC
#define STYPE   0b00000
#define STYPE_L 0b00000
#define STYPE_P 0b10000

// To be used with REGIMM_OP as rt field
#define BGEZ_RT 0b00001
#define BGEZAL_RT 0b10001
#define BGEZALL_RT 0b10011
#define BGEZL_RT 0b00011
#define BLTZ_RT 0b00000
#define BLTZAL_RT 0b10000
#define BLTZALL_RT 0b10010
#define BLTZL_RT 0b00010
#define TEQI_RT 0b01100
#define TGEI_RT 0b01000
#define TGEIU_RT 0b01001
#define TLTI_RT 0b01010
#define TLTIU_RT 0b01011
#define TNEI_RT 0b01110
#define MTSAB_RT 0b11000
#define MTSAH_RT 0b11001

#define JMP_INSTRUCTION(op, target) ((op << 26) | target)
#define REG_INSTRUCTION(op, rs, rt, rd, sa, fun) ((op << 26) | (rs << 21) | (rt << 16) | (rd << 11) | (sa << 6) | fun)
#define IMM_INSTRUCTION(op, rs, rt, imm) ((op << 26) | (rs << 21) | (rt << 16) | imm)

#define ADD(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, ADD_FUNC)
#define ADDI(rs, rt, imm) IMM_INSTRUCTION(SPECIAL_OP, rs, rt, imm)
#define ADDIU(rs, rt, imm) IMM_INSTRUCTION(ADDIU_OP, rs, rt, imm)
#define ADDU(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, ADDU_FUNC)
#define AND(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, AND_FUNC)
#define ANDI(rs, rt, imm) IMM_INSTRUCTION(ANDI_OP, rs, rt, imm)
#define BEQ(rs, rt, offset) IMM_INSTRUCTION(BEQ_OP, rs, rt, offset)
#define BEQL(rs, rt, offset) IMM_INSTRUCTION(BEQL_OP, rs, rt, offset)
#define BGEZ(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BGEZ_RT, offset)
#define BGEZAL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BGEZAL_RT, offset)
#define BGEZALL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BGEZALL_RT, offset)
#define BGEZL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BGEZL_RT, offset)
#define BGTZ(rs, offset) IMM_INSTRUCTION(BGTZ_OP, rs, 0b00000, offset)
#define BGTZL(rs, offset) IMM_INSTRUCTION(BGTZL_OP, rs, 0b00000, offset)
#define BLEZ(rs, offset) IMM_INSTRUCTION(BLEZ_OP, rs, 0b00000, offset)
#define BGTZL(rs, offset) IMM_INSTRUCTION(BGTZL_OP, rs, 0b00000, offset)
#define BLEZ(rs, offset) IMM_INSTRUCTION(BLEZ_OP, rs, 0b00000, offset)
#define BLEZL(rs, offset) IMM_INSTRUCTION(BLEZL_OP, rs, 0b00000, offset)
#define BLTZ(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BLTZ_RT, offset)
#define BLTZAL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BLTZAL_RT, offset)
#define BLTZALL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BLTZALL_RT, offset)
#define BLTZL(rs, offset) IMM_INSTRUCTION(REGIMM_OP, rs, BLTZL_RT, offset)
#define BNE(rs, rt, offset) IMM_INSTRUCTION(BNE_OP, rs, rt, offset)
#define BNEL(rs, rt, offset) IMM_INSTRUCTION(BNEL_OP, rs, rt, offset)
// #define BREAK(code)
#define DADD(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DADD_FUNC)
#define DADDI(rs, rt, offset) IMM_INSTRUCTION(DADDI_OP, rs, rt, offset)
#define DADDIU(rs, rt, offset) IMM_INSTRUCTION(DADDIU_OP, rs, rt, offset)
#define DADDU(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DADDU_FUNC)
#define DIV(rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, 0b00000, 0b00000, DIV_FUNC)
#define DIVU(rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, 0b00000, 0b00000, DIVU_FUNC)
#define DSLL(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSLL_FUNC)
#define DSLL32(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSLL32_FUNC)
#define DSLLV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DSLLV_FUNC)
#define DSRA(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSRA_FUNC)
#define DSRA32(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSRA32_FUNC)
#define DSRAV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DSRAV_FUNC)
#define DSRL(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSRL_FUNC)
#define DSRL32(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, DSRL32_FUNC)
#define DSRLV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DSRLV_FUNC)
#define DSUB(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DSUB_FUNC)
#define DSUBU(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, DSUBU_FUNC)
#define J(offset) JMP_INSTRUCTION(J_OP, offset)
#define JAL(offset) JMP_INSTRUCTION(JAL_OP, offset)
#define JALR(rd, rs) REG_INSTRUCTION(SPECIAL_OP, rs, 0b00000, rd, 0b00000, JALR_FUNC)
#define JR(rs) REG_INSTRUCTION(SPECIAL_OP, rs, 0b00000, 0b00000, 0b00000, JR_FUNC)
#define LB(rt, base, offset) IMM_INSTRUCTION(LB_OP, base, rt, offset)
#define LBU(rt, base, offset) IMM_INSTRUCTION(LBU_OP, base, rt, offset)
#define LD(rt, base, offset) IMM_INSTRUCTION(LD_OP, base, rt, offset)
#define LDL(rt, base, offset) IMM_INSTRUCTION(LDL_OP, base, rt, offset)
#define LDR(rt, base, offset) IMM_INSTRUCTION(LDR_OP, base, rt, offset)
#define LH(rt, base, offset) IMM_INSTRUCTION(LH_OP, base, rt, offset)
#define LHU(rt, base, offset) IMM_INSTRUCTION(LHU_OP, base, rt, offset)
#define LUI(rt, immediate) IMM_INSTRUCTION(LUI_OP, 0b00000, rt, immediate)
#define LW(rt, base, offset) IMM_INSTRUCTION(LW_OP, base, rt, offset)
#define LWL(rt, base, offset) IMM_INSTRUCTION(LWL_OP, base, rt, offset)
#define LWR(rt, base, offset) IMM_INSTRUCTION(LWR_OP, base, rt, offset)
#define LWU(rt, base, offset) IMM_INSTRUCTION(LWU_OP, base, rt, offset)
#define MFHI(rd) REG_INSTRUCTION(SPECIAL_OP, 0b00000, 0b00000, rd, 0b00000, MFHI_FUNC)
#define MFLO(rd) REG_INSTRUCTION(SPECIAL_OP, 0b00000, 0b00000, rd, 0b00000, MFLO_FUNC)
#define MOVN(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, MOVN_FUNC)
#define MOVZ(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, MOVZ_FUNC)
#define MTHI(rs) REG_INSTRUCTION(SPECIAL_OP, rs, 0b00000, 0b00000, 0b00000, MTHI_FUNC)
#define MTLO(rs) REG_INSTRUCTION(SPECIAL_OP, rs, 0b00000, 0b00000, 0b00000, MTLO_FUNC)
#define MULT(rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, 0b00000, 0b00000, MULT_FUNC)
#define MULTU(rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, 0b00000, 0b00000, MULTU_FUNC)
#define NOR(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, NOR_FUNC)
#define OR(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, OR_FUNC)
#define ORI(rt, rs, immediate) IMM_INSTRUCTION(ORI_OP, rs, rt, immediate)
#define PREF(hint, base, offset) IMM_INSTRUCTION(PREF_OP, base, hint, offset)
#define SB(rt, base, offset) IMM_INSTRUCTION(SB_OP, base, rt, offset)
#define SD(rt, base, offset) IMM_INSTRUCTION(SD_OP, base, rt, offset)
#define SDL(rt, base, offset) IMM_INSTRUCTION(SDL_OP, base, rt, offset)
#define SDR(rt, base, offset) IMM_INSTRUCTION(SDR_OP, base, rt, offset)
#define SH(rt, base, offset) IMM_INSTRUCTION(SH_OP, base, rt, offset)
#define SLL(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, SLL_FUNC)
#define SLLV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SLLV_FUNC)
#define SLT(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SLT_FUNC)
#define SLTI(rt, ts, immediate) IMM_INSTRUCTION(SLTI_OP, rs, rt, immediate)
#define SLTIU(rt, ts, immediate) IMM_INSTRUCTION(SLTIU_OP, rs, rt, immediate)
#define SLTU(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SLTU_FUNC)
#define SRA(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, SRA_FUNC)
#define SRAV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SRAV_FUNC)
#define SRL(rd, rt, sa) REG_INSTRUCTION(SPECIAL_OP, 0b00000, rt, rd, sa, SRL_FUNC)
#define SRLV(rd, rt, rs) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SRLV_FUNC)
#define SUB(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SUB_FUNC)
#define SUBU(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, SUBU_FUNC)
#define SW(rt, base, offset) IMM_INSTRUCTION(SW_OP, base, rt, offset)
#define SWL(rt, base, offset) IMM_INSTRUCTION(SWL_OP, base, rt, offset)
#define SWR(rt, base, offset) IMM_INSTRUCTION(SWR_OP, base, rt, offset)
#define SYNC(stype) REG_INSTRUCTION(SPECIAL_OP, 0b00000, 0b00000, 0b00000, stype, SYNC_FUNC)
// #define SYSCALL(code)
// #define TEQ(rs, rt)
#define TEQI(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TEQI_RT, immediate)
// #define TGE(rs, rt)
#define TGEI(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TGEI_RT, immediate)
#define TGEIU(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TGEIU_RT, immediate)
// #define TGEU(rs, rt)
// #define TLT(rs, rt)
#define TLTI(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TLTI_RT, immediate)
#define TLTIU(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TLTIU_RT, immediate)
// #define TLTU(rs, rt)
// #define TNE(rs, rt)
#define TNEI(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, TNEI_RT, immediate)
#define XOR(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, XOR_FUNC)
#define XORI(rt, rs, immediate) IMM_INSTRUCTION(XORI_OP, rs, rt, immediate)
#define DIV1(rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, 0b00000, 0b00000, DIV1_FUNC)
#define DIVU1(rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, 0b00000, 0b00000, DIVU1_FUNC)
#define LQ(rt, base, offset) IMM_INSTRUCTION(LQ_OP, base, rt, offset)
#define MADD(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MADD_FUNC)
#define MADD1(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MADD1_FUNC)
#define MADDU(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MADDU_FUNC)
#define MADDU1(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MADDU1_FUNC)
#define MFHI1(rd) REG_INSTRUCTION(MMI_OP, 0b00000, 0b00000, rd, 0b00000, MFHI1_FUNC)
#define MFLO1(rd) REG_INSTRUCTION(MMI_OP, 0b00000, 0b00000, rd, 0b00000, MFLO1_FUNC)
#define MFSA(rd) REG_INSTRUCTION(SPECIAL_OP, 0b00000, 0b00000, rd, 0b00000, MFSA_FUNC)
#define MTHI1(rs) REG_INSTRUCTION(MMI_OP, rs, 0b00000, 0b00000, 0b00000, MTHI1_FUNC)
#define MTLO1(rs) REG_INSTRUCTION(MMI_OP, rs, 0b00000, 0b00000, 0b00000, MTLO1_FUNC)
#define MTSA(rs) REG_INSTRUCTION(SPECIAL_OP, rs, 0b00000, 0b00000, 0b00000, MTSA_FUNC)
#define MTSAB(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, MTSAB_RT, immediate)
#define MTSAH(rs, immediate) IMM_INSTRUCTION(REGIMM_OP, rs, MTSAH_RT, immediate)
#define MULT(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, MULT_FUNC)
#define MULT1(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MULT1_FUNC)
#define MULTU(rd, rs, rt) REG_INSTRUCTION(SPECIAL_OP, rs, rt, rd, 0b00000, MULTU_FUNC)
#define MULTU1(rd, rs, rt) REG_INSTRUCTION(MMI_OP, rs, rt, rd, 0b00000, MULTU1_FUNC)

// TODO: Add opcodes from PABSH 

typedef enum {
    zero, // always 0
    at, // Reserved for pseudo-instructions
    v0, v1, // Return values from funtions
    a0, a1, a2, a3, // Arguments to functions
    t0, t1, t2, t3, t4, t5, t6, t7, // Temporary regs
    s0, s1, s2, s3, s4, s5, s6, s7, // Saved regs
    t8, t9,  // Temporary regs
    k0, k1, // Kernel regs, DON'T USE!
    gp, // Global Area Pointer
    sp, // Stack Pointer
    fp, // Frame Pointer
    ra // Return Address
} gpr_idx;

// TODO: Add FPR regs

typedef struct {
    unsigned int op : 6;      // Opcode
    unsigned int target : 26; // Jump target address
} jmp_instruction_t;

typedef struct {
    unsigned int op : 6;   // Opcode
    unsigned int rs : 5;   // Source register specifier
    unsigned int rt : 5;   // Specifies target (source/destination) register or branch condition
    unsigned int imm : 16; // Immediate value, branch instruction offset or offset address
} imm_instruction_t;

typedef struct {
    unsigned int op : 6;  // Opcode
    unsigned int rs : 5;  // Source register specifier
    unsigned int rt : 5;  // Specifies target (source/destination) register or branch condition
    unsigned int rd : 5;  // Destination register specifier
    unsigned int sa : 5;  // Shift amount
    unsigned int fun : 6; // Function field
} reg_instruction_t;
