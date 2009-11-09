(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-info-libtag)

(add-hook 'emms-player-started-hook 'emms-show)

(setq
	emms-player-list '(emms-player-mplayer)
	emms-info-functions '(emms-info-libtag)
	emms-show-format "NP: %s")
