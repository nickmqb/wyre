top(clk $1, button $1) {
	out reg leds $3

	posedge clk {
		if button {
			leds <= '111
		} else {
			leds <= inc(a: leds).o
		}
	}
}

inc(a $3) {
	out o := a + 1
}
