# 中文算法 LaTeX 项目

主文件：`软件学报.tex`

依赖宏包：`fontspec`、`xeCJK`、`algorithm2e`、`amsmath`。

系统需要安装 XeLaTeX 和中文宏包，例如：

```bash
sudo apt-get install texlive-xetex texlive-lang-chinese
```

在本目录执行：

```bash
xelatex 软件学报.tex
xelatex 软件学报.tex
```

生成的 `软件学报.pdf` 可直接用于查看排版和截取算法图片。第二次编译用于
更新算法编号及交叉引用。
