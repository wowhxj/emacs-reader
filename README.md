# emacs-reader

[hectorqin/reader](https://github.com/hectorqin/reader) 的轻量 Emacs 客户端：最近阅读优先的卡片书架、中文段落排版、居中阅读与章节切换。

目前支持 Reader3（`/reader3/`）文本与 EPUB 章节阅读（EPUB 需 Emacs 带 libxml 支持），**尚未实现进度同步和 EmuReader 集成**。已在 Emacs 32.0.50 验证。

## 安装与配置

Emacs 30+ 可用内置 `use-package` 的 `:vc` 从 GitHub 安装：

```elisp
(use-package emacs-reader
  :vc (:url "https://github.com/wowhxj/emacs-reader"
       :rev :newest)
  :commands (emacs-reader-login emacs-reader-shelf emacs-reader-search)
  :bind ("C-c r" . emacs-reader-shelf)
  :custom
  (emacs-reader-server-url "https://reader.example.com")
  (emacs-reader-username "your-username")
  (emacs-reader-text-width 80) ; 约 40 个汉字
  :config
  ;; 推荐加密保存密码，不要将密码写进配置。
  (add-to-list 'auth-sources "~/.authinfo.gpg"))
```

首次运行 `M-x emacs-reader-login`，输入服务器地址、用户名和密码；登录成功后可确认保存密码。地址和用户名通过 Customize 保存，密码由 `auth-source` 管理；以后打开书架会自动登录，会话过期会尝试重新登录。GPG 解密可能需要口令。

服务器地址和用户名也可以不预先配置，登录时会提示输入。默认允许 HTTP，但会明文传输凭据；设置 `emacs-reader-allow-insecure-http` 为 `nil` 可禁止。

## 使用

`M-x emacs-reader-shelf` 打开书架，选择一本书按 `RET` 阅读。书架、正文和目录统一在当前 frame 的 `emacs-reader` tab 中打开；首次自动创建，之后复用，不改变其他 tab 的窗口布局。顶部显示章节名和全书进度（按章节序号估算，非字数进度）。

在书架、正文或目录按 `q` 会关闭（kill）当前 buffer，而非仅隐藏；关闭正文时也会停止朗读并清理音频资源。tab 本身保留。

| 界面 | 按键 | 操作 |
|---|---|---|
| 书架 | `n` / `p` | 下一本 / 上一本 |
| 书架 | `g` | 刷新 |
| 书架 | `s` | 在服务器配置的书源中搜索 |
| 搜索 | `n` / `p` | 下一条 / 上一条结果 |
| 搜索 | `RET` | 试读，不加入书架 |
| 搜索 | `a` | 确认后加入服务器书架 |
| 搜索 | `m` / `g` | 搜索下一批；失败时重试当前批 |
| 搜索 | `s` | 输入新关键词并选择搜索范围 |
| 阅读 | `a` | 将当前试读书籍加入书架 |
| 阅读 | `j` / `k` | 向下 / 向上滚一行 |
| 阅读 | `d` / `u` | 向下 / 向上翻半屏 |
| 阅读 | `n` / `p` | 向下 / 向上翻整屏；`n` 在章末进入下一章 |
| 阅读 | `SPC` | 向下翻整屏，章末进入下一章 |
| 阅读 | `]` / `[` | 下一章 / 上一章 |
| 阅读 | `c` | 打开目录，`▶ 当前` 标记正在阅读的章节 |
| 阅读 | `C` | 按章节编号跳转 |
| 阅读 | `r` | 开始 / 停止逐段朗读，播放段落高亮 |
| 目录 | `n` / `p` | 下一项 / 上一项 |
| 目录 | `RET` | 跳转选中章节 |
| 全部 | `q` | 关闭当前 buffer |

朗读复用服务器 HttpTTS 配置，默认声音为“晓晓”，可通过 `emacs-reader-tts-voice` 修改，`emacs-reader-tts-rate` 调节语速。需本机有 `afplay`（macOS 自带）、`mpv` 或 `ffplay`。从光标所在段落读到本章结束，光标跟随正在播放的段落，后台播放也会记录位置，切回时恢复光标；停止后再按 `r` 从该段重新开始；播放时预取下一段以减少停顿；切章或关闭缓冲区自动停止并清理预取，不同步服务器进度。

WSL2 + WSLg 可安装 Linux 侧 `mpv` 播放。没有 Linux 音频输出时，可在 Windows 安装 `mpv` 或 `ffplay` 并加入 PATH，WSL 中确认 `command -v mpv.exe` 或 `command -v ffplay.exe` 能找到；客户端会用 `wslpath` 转换音频路径。若需要强制使用 Windows 播放器：

```elisp
(setq emacs-reader-tts-player-command '("mpv.exe" "--no-video" "--really-quiet"))
```

## 书源搜索与试读

`M-x emacs-reader-search`（或在书架按 `s`）输入书名/作者后，从服务器读取已配置的书源，选择单源、全部书源或一个书源分组。这里调用书源搜索，不是过滤现有书架。服务器需已配置可用书源；搜索成败取决于书源站点和规则。

搜索结果显示书名、作者、书源、最新章节。`RET` 先带书源信息初始化目录，再打开首章；可使用现有目录、翻章及朗读功能。试读不发送 `saveBook` 或进度保存请求；服务器仍会处理搜索及章节缓存。`a` 经确认后才写入服务器书架，已打开的书架会刷新。

搜索使用部署版 Web 已有的 `searchBook` / `searchBookMulti` JSON 接口，默认多源并发数为 4；`m` 继续下一批，失败时 `g` 重试而不丢失已有结果。多源搜索中一批无结果但源游标仍前进时，可以继续搜索。与 Web 的展示区别：此版本按书源分别列出同一本书，便于直接选择试读来源，不折叠为一个“来源数量”条目；不使用 SSE 流式展示，等待当前批返回后显示。

## Android 本地 EPUB 如何跨端阅读

**Android 能打开某个本地文件，不代表 Reader 服务器已拥有它。** 仅凭文件名或 Android 书架记录无法确认上传状态；同步书架、备份或进度也不能代替上传 EPUB 文件。尤其 Android 的本地路径不能由远端服务器或另一台电脑直接读取。

建议按以下顺序操作：

1. 从 Android 的原文件位置、文件管理器或客户端支持的导出方式，取得原始 EPUB（不是只导出书架 JSON）。Android 客户端的具体导出入口未在本项目验证。
2. 登录 Reader **Web 端同一个账号**，查看书架是否已有该书，并尝试打开目录和正文。能打开才说明 Web 端实际可读；仅有书架条目并不足以证明文件可用。
3. 如果 Web 没有该书，或条目指向手机本地文件，在 Web 设置中使用本地书籍导入入口选择原始 EPUB。部署版 Web 的实际流程是 `importBookPreview` 上传文件、显示“导入本地书籍”预览，再通过 `saveBook` 确认导入；不要停在预览对话框就认为已加入书架。
4. 确认 Web 可以阅读后，在 Emacs 运行 `emacs-reader-shelf`（已有书架按 `g`），选择该书按 `RET`。客户端通过服务器的 `getChapterList` / `getBookContent` 读取，EPUB 章节资源由现有 HTML 渲染路径显示，要求 Emacs 带 libxml；无需另在 Emacs 下载/导入同一文件。

若 Web 端有条目却读不了，先确认文件上传完成、账号一致、服务器文件仍在以及目录解析成功；不要直接修改书架里的手机路径或反复加书。本客户端本次未增加 EPUB 上传功能，也未实现跨端进度同步；重新导入的书籍不保证与 Android 原记录共享标识或阅读位置。服务器上的文件及其持久化存储需要另行备份。

## 开发验证

```sh
emacs -Q --batch -L . -l test/emacs-reader-test.el -f ert-run-tests-batch-and-exit
```

测试使用合成书籍数据和本机 HTTP fixture，不向真实服务器添加或修改书籍。搜索契约依据部署版 Web `v3.2.14-05250935` 的公开前端源码核对；尚未进行真实账号的搜索/试读/入架端到端验收。

`M-x customize-group RET emacs-reader` 可调整阅读宽度、书架字体和颜色。
