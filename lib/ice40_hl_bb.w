RAM256x16(
    rclk $1
    wclk $1
    raddr $8
    waddr $8
    wdata $16
    we $1
    #initial_data $4096
) {
    out rdata := RAM.RDATA

    RAM := SB_RAM40_4K(
        #READ_MODE: 0
        #WRITE_MODE: 0
        #INIT_0: chunk(#initial_data, 0, 16)
        #INIT_1: chunk(#initial_data, 1, 16)
        #INIT_2: chunk(#initial_data, 2, 16)
        #INIT_3: chunk(#initial_data, 3, 16)
        #INIT_4: chunk(#initial_data, 4, 16)
        #INIT_5: chunk(#initial_data, 5, 16)
        #INIT_6: chunk(#initial_data, 6, 16)
        #INIT_7: chunk(#initial_data, 7, 16)
        #INIT_8: chunk(#initial_data, 8, 16)
        #INIT_9: chunk(#initial_data, 9, 16)
        #INIT_A: chunk(#initial_data, 10, 16)
        #INIT_B: chunk(#initial_data, 11, 16)
        #INIT_C: chunk(#initial_data, 12, 16)
        #INIT_D: chunk(#initial_data, 13, 16)
        #INIT_E: chunk(#initial_data, 14, 16)
        #INIT_F: chunk(#initial_data, 15, 16)
        RCLK: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'xxx, raddr }
        WCLK: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'xxx, waddr }
        WDATA: wdata
    )
}

RAM256x16_N(
    rclk $1
    wclk $1
    raddr $8
    waddr $8
    wdata $16
    we $1
    #initial_data $4096
) {
    out rdata := RAM.RDATA

    RAM := SB_RAM40_4KNRNW(
        #READ_MODE: 0
        #WRITE_MODE: 0
        #INIT_0: chunk(#initial_data, 0, 16)
        #INIT_1: chunk(#initial_data, 1, 16)
        #INIT_2: chunk(#initial_data, 2, 16)
        #INIT_3: chunk(#initial_data, 3, 16)
        #INIT_4: chunk(#initial_data, 4, 16)
        #INIT_5: chunk(#initial_data, 5, 16)
        #INIT_6: chunk(#initial_data, 6, 16)
        #INIT_7: chunk(#initial_data, 7, 16)
        #INIT_8: chunk(#initial_data, 8, 16)
        #INIT_9: chunk(#initial_data, 9, 16)
        #INIT_A: chunk(#initial_data, 10, 16)
        #INIT_B: chunk(#initial_data, 11, 16)
        #INIT_C: chunk(#initial_data, 12, 16)
        #INIT_D: chunk(#initial_data, 13, 16)
        #INIT_E: chunk(#initial_data, 14, 16)
        #INIT_F: chunk(#initial_data, 15, 16)
        RCLKN: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'xxx, raddr }
        WCLKN: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'xxx, waddr }
        WDATA: wdata
    )
}

RAM512x8(
    rclk $1
    wclk $1
    raddr $9
    waddr $9
    wdata $8
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[14], rd[12], rd[10], rd[8], rd[6], rd[4], rd[2], rd[0] }

    #initial_data_sw := swizzle(#initial_data, 1, 2048, 4096)

    RAM := SB_RAM40_4K(
        #READ_MODE: 1
        #WRITE_MODE: 1
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLK: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'xx, raddr }
        WCLK: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'xx, waddr }
        WDATA: {
            'x, wdata[7], 'x, wdata[6], 'x, wdata[5], 'x, wdata[4],
            'x, wdata[3], 'x, wdata[2], 'x, wdata[1], 'x, wdata[0]
        }
    )
}

RAM512x8_N(
    rclk $1
    wclk $1
    raddr $9
    waddr $9
    wdata $8
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[14], rd[12], rd[10], rd[8], rd[6], rd[4], rd[2], rd[0] }

    #initial_data_sw := swizzle(#initial_data, 1, 2048, 4096)

    RAM := SB_RAM40_4KNRNW(
        #READ_MODE: 1
        #WRITE_MODE: 1
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLKN: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'xx, raddr }
        WCLKN: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'xx, waddr }
        WDATA: {
            'x, wdata[7], 'x, wdata[6], 'x, wdata[5], 'x, wdata[4],
            'x, wdata[3], 'x, wdata[2], 'x, wdata[1], 'x, wdata[0]
        }
    )
}

RAM1024x4(
    rclk $1
    wclk $1
    raddr $10
    waddr $10
    wdata $4
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[13], rd[9], rd[5], rd[1] }

    #initial_data_sw := swizzle(#initial_data, 1, 1024, 4096)

    RAM := SB_RAM40_4K(
        #READ_MODE: 2
        #WRITE_MODE: 2
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLK: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'x, raddr }
        WCLK: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'x, waddr }
        WDATA: { 'xx, wdata[3], 'xxx, wdata[2], 'xxx, wdata[1], 'xxx, wdata[0], 'x }
    )
}

RAM1024x4_N(
    rclk $1
    wclk $1
    raddr $10
    waddr $10
    wdata $4
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[13], rd[9], rd[5], rd[1] }

    #initial_data_sw := swizzle(#initial_data, 1, 1024, 4096)

    RAM := SB_RAM40_4KNRNW(
        #READ_MODE: 2
        #WRITE_MODE: 2
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLKN: rclk
        RCLKE: 1
        RE: 1
        RADDR: { 'x, raddr }
        WCLKN: wclk
        WCLKE: 1
        WE: we
        WADDR: { 'x, waddr }
        WDATA: { 'xx, wdata[3], 'xxx, wdata[2], 'xxx, wdata[1], 'xxx, wdata[0], 'x }
    )
}

RAM2048x2(
    rclk $1
    wclk $1
    raddr $11
    waddr $11
    wdata $2
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[11], rd[3] }

    #initial_data_sw := swizzle(#initial_data, 1, 512, 4096)

    RAM := SB_RAM40_4K(
        #READ_MODE: 3
        #WRITE_MODE: 3
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLK: rclk
        RCLKE: 1
        RE: 1
        RADDR: raddr
        WCLK: wclk
        WCLKE: 1
        WE: we
        WADDR: waddr
        WDATA: { 'xxxx, wdata[1], 'xxxxxxx, wdata[0], 'xxx }
    )
}

RAM2048x2_N(
    rclk $1
    wclk $1
    raddr $11
    waddr $11
    wdata $2
    we $1
    #initial_data $4096
) {
	rd := RAM.RDATA
    out rdata := { rd[11], rd[3] }

    #initial_data_sw := swizzle(#initial_data, 1, 512, 4096)

    RAM := SB_RAM40_4KNRNW(
        #READ_MODE: 3
        #WRITE_MODE: 3
        #INIT_0: chunk(#initial_data_sw, 0, 16)
        #INIT_1: chunk(#initial_data_sw, 1, 16)
        #INIT_2: chunk(#initial_data_sw, 2, 16)
        #INIT_3: chunk(#initial_data_sw, 3, 16)
        #INIT_4: chunk(#initial_data_sw, 4, 16)
        #INIT_5: chunk(#initial_data_sw, 5, 16)
        #INIT_6: chunk(#initial_data_sw, 6, 16)
        #INIT_7: chunk(#initial_data_sw, 7, 16)
        #INIT_8: chunk(#initial_data_sw, 8, 16)
        #INIT_9: chunk(#initial_data_sw, 9, 16)
        #INIT_A: chunk(#initial_data_sw, 10, 16)
        #INIT_B: chunk(#initial_data_sw, 11, 16)
        #INIT_C: chunk(#initial_data_sw, 12, 16)
        #INIT_D: chunk(#initial_data_sw, 13, 16)
        #INIT_E: chunk(#initial_data_sw, 14, 16)
        #INIT_F: chunk(#initial_data_sw, 15, 16)
        RCLKN: rclk
        RCLKE: 1
        RE: 1
        RADDR: raddr
        WCLKN: wclk
        WCLKE: 1
        WE: we
        WADDR: waddr
        WDATA: { 'xxxx, wdata[1], 'xxxxxxx, wdata[0], 'xxx }
    )
}

RAM256K(
	clk $1
	addr $14
	wdata $16
	wmask $4
	we $1
) {
	out rdata := RAM.DATAOUT

	RAM := SB_SPRAM256KA(
		CLOCK: clk
		ADDRESS: addr
		DATAIN: wdata
		MASKWREN: wmask
		WREN: we
		CHIPSELECT: '1
		STANDBY: '0
		SLEEP: '0
		POWEROFF: '1
	)
}

MUL16x16(
	clk $1
	a $16
	b $16
) {
	mac := SB_MAC16(
		CLK: clk
		CE: ---
		C: ---
		A: a
		B: b
		D: ---
		AHOLD: '0
		BHOLD: '0
		CHOLD: '0
		DHOLD: '0
		IRSTTOP: '0
		IRSTBOT: '0
		ORSTTOP: '0
		ORSTBOT: '0
		OLOADTOP: '0
		OLOADBOT: '0
		ADDSUBTOP: '0
		ADDSUBBOT: '0
		OHOLDTOP: '0
		OHOLDBOT: '0
		CI: '0
		ACCUMCI: '0
		SIGNEXTIN: '0
		#B_SIGNED: '0
		#A_SIGNED: '0
		#MODE_8x8: '0
		#BOTADDSUB_CARRYSELECT: '00
		#BOTADDSUB_UPPERINPUT: '0
		#BOTADDSUB_LOWERINPUT: '00
		#BOTOUTPUT_SELECT: '11
		#TOPADDSUB_CARRYSELECT: '00
		#TOPADDSUB_UPPERINPUT: '0
		#TOPADDSUB_LOWERINPUT: '00
		#TOPOUTPUT_SELECT: '11
		#PIPELINE_16x16_MULT_REG2: '0
		#PIPELINE_16x16_MULT_REG1: '0
		#BOT_8x8_MULT_REG: '0
		#TOP_8x8_MULT_REG: '0
		#D_REG: '0
		#B_REG: '0
		#A_REG: '0
		#C_REG: '0
		#NEG_TRIGGER: '0
	)

	out o := mac.O
}

MUL16x16_SIGNED(
	clk $1
	a $16
	b $16
) {
	mac := SB_MAC16(
		CLK: clk
		CE: ---
		C: ---
		A: a
		B: b
		D: ---
		AHOLD: '0
		BHOLD: '0
		CHOLD: '0
		DHOLD: '0
		IRSTTOP: '0
		IRSTBOT: '0
		ORSTTOP: '0
		ORSTBOT: '0
		OLOADTOP: '0
		OLOADBOT: '0
		ADDSUBTOP: '0
		ADDSUBBOT: '0
		OHOLDTOP: '0
		OHOLDBOT: '0
		CI: '0
		ACCUMCI: '0
		SIGNEXTIN: '0
		#B_SIGNED: '1
		#A_SIGNED: '1
		#MODE_8x8: '0
		#BOTADDSUB_CARRYSELECT: '00
		#BOTADDSUB_UPPERINPUT: '0
		#BOTADDSUB_LOWERINPUT: '00
		#BOTOUTPUT_SELECT: '11
		#TOPADDSUB_CARRYSELECT: '00
		#TOPADDSUB_UPPERINPUT: '0
		#TOPADDSUB_LOWERINPUT: '00
		#TOPOUTPUT_SELECT: '11
		#PIPELINE_16x16_MULT_REG2: '0
		#PIPELINE_16x16_MULT_REG1: '0
		#BOT_8x8_MULT_REG: '0
		#TOP_8x8_MULT_REG: '0
		#D_REG: '0
		#B_REG: '0
		#A_REG: '0
		#C_REG: '0
		#NEG_TRIGGER: '0
	)

	out o := mac.O
}
