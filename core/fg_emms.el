(require 'emms-source-file)
(require 'emms-source-playlist)
(require 'emms-player-mplayer)
(require 'emms-playlist-mode)
(require 'emms-info-libtag)

(add-hook 'emms-player-started-hook 'emms-show)

(setq
	emms-player-list
		'(emms-player-mplayer-playlist emms-player-mplayer)
	emms-info-functions '(emms-info-libtag)
	emms-show-format "NP: %s")
