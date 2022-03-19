## hook for direnv
set @edit:before-readline = $@edit:before-readline {
	try {
		var m = [(e:direnv export elvish | from-json)]
		if (> (count $m) 0) {
			set m = (all $m)
			keys $m | each {|k|
				if $m[$k] {
					set-env $k $m[$k]
				} else {
					unset-env $k
				}
			}
		}
	} except e {
		echo $e
	}
}
