:crashAllocator IAllocator #Mutable

CrashHandler {
	enable() {
		::crashAllocator = Memory.newArenaAllocator(64 * 1024)
		signal(SIGSEGV, pointer_cast(crashHandler, pointer))
	}

	crashHandler(sig int) {
		::currentAllocator = ::crashAllocator
		prctl(PR_SET_PTRACER, PR_SET_PTRACER_ANY, 0, 0, 0)
		system("xfce4-terminal --hold -x gdb -tui -p $PPID")
		while true {
			usleep(1000_u * 1000_u)
		}
	}
}
