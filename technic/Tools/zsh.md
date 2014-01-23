# zsh实践

# 配置:

使用开源项目oh-myzsh:

	git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
	cp ~/.oh-my-zsh/templates/zshrc.zsh-template ~/.zshrc
	chsh -s /bin/zsh
	//重启系统

zsh + oh-my-zsh + powerline

# 使用solarized配色

下载地址: http://ethanschoonover.com/solarized

配置vim:

	$ cd solarized
	$ cd vim-colors-solarized/colors
	$ mkdir -p ~/.vim/colors
	$ cp solarized.vim ~/.vim/colors/

	$ vi ~/.vimrc
	syntax enable
	set background=dark
	colorscheme solarized

	