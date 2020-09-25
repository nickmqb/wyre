// Scrolling test pattern for VGA mode 640x480x60hz

core(
	clk $1 // clk must be 25.175mhz
) {
	posedge clk {
		reg x $10		
		reg y $10
		reg frame $8
		if x == 799 {
			x <= 0
		} else {
			x <= x + 1
		}
		if x == 639 {
			if y == 524 {
				y <= 0
				frame <= frame + 1
			} else {
				y <= y + 1
			}
		}
		
		reg vis_x $1
		reg vis_y $1
		reg hs $1
		reg vs $1
		if x == 0 {
			vis_x <= 1
		}
		if x == 640 {
			vis_x <= 0
		}
		if x == 656 {
			hs <= 1
		}
		if x == 752 {
			hs <= 0
		}
		if y == 0 {
			vis_y <= 1
		}
		if y == 480 {
			vis_y <= 0
		}
		if y == 490 {
			vs <= 1
		}
		if y == 492 {
			vs <= 0
		}
		x_offset := x[5:0] + frame[5:0]
		reg color <= { frame[7], y[5], x_offset[5] }
		
		vis := vis_x & vis_y

		// 4-bit output per color component
		out vga_r := vis ? rep(color[0], 4) : '0000
		out vga_g := vis ? rep(color[1], 4) : '0000
		out vga_b := vis ? rep(color[2], 4) : '0000
		out vga_hs := ~hs // hsync
		out vga_vs := ~vs // vsync
	}
}
