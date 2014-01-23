# vim阅读代码

# 1. 使用cscope

$ find . -name "*.h" >  cscope.files
$ find . -name "*.cpp" >> cscope.files

vim 配置:

    map <leader>fs :call CscopeFind('s', expand('<cword>'))<CR>  // ,fs 查找标志Symbol
    map <leader>fg :call CscopeFind('g', expand('<cword>'))<CR>  // ,fg 查找定义
    map <leader>fd :call CscopeFind('d', expand('<cword>'))<CR>  // ,fd 查找被这个函数调用的方法
    map <leader>fc :call CscopeFind('c', expand('<cword>'))<CR>  // ,fc 查找这个函数调用的方法.
    map <leader>ft :call CscopeFind('t', expand('<cword>'))<CR>  // ,ft 查找这个字符串
    map <leader>fe :call CscopeFind('e', expand('<cword>'))<CR>  // ,fe 查找egrep pattern
    map <leader>ff :call CscopeFind('f', expand('<cword>'))<CR>  // ,ff 查找文件
    map <leader>fi :call CscopeFind('i', expand('<cword>'))<CR>  // ,fi 查找那些#include该文件的文件列表

# 2. 其它跳转

- [[ 上一个方法头.
- ]] 下个方法头
- }} 下一个段落
- {{ 上一个段落

# 3. 使用WinManager和NERDTree.

vim 配置:

    let g:NERDTree_title="[NERD Tree]" 
    let g:winManagerWindowLayout='NERDTree|TagList,BufExplorer'
    function! NERDTree_Start()
        exec 'NERDTree'
    endfunction
    function! NERDTree_IsValid()
        return 1
    endfunction
    nmap wm :if IsWinManagerVisible() <BAR> WMToggle<CR> <BAR> else <BAR> WMToggle<CR>:q<CR> endif <CR><CR>

NERDTree 快捷键:

- o:     在已有窗口中打开文件、目录或书签，并跳到该窗口 
- O:     递归打开选中 结点下的所有目录
- t:     在新Tab中打开选中文件/书签，并跳到新Tab
- x:     合拢选中结点的父目录
- X:     递归合拢选中结点下的所有目录
- P:     跳到根节点
- p:     跳到父节点


