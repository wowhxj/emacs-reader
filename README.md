# emacs-reader

[hectorqin/reader](https://github.com/hectorqin/reader) 的轻量 Emacs 客户端：最近阅读优先的卡片书架、中文段落排版、居中阅读与章节切换。

目前支持 Reader3（`/reader3/`）文本与 EPUB 章节阅读（EPUB 需 Emacs 带 libxml 支持），**尚未实现进度同步和 EmuReader 集成**。已在 Emacs 32.0.50 验证。

## 安装与配置

Emacs 30+ 可用内置 `use-package` 的 `:vc` 从 GitHub 安装：

```elisp
(use-package emacs-reader
  :vc (:url "https://github.com/wowhxj/emacs-reader"
       :rev :newest)
  :commands (emacs-reader-login emacs-reader-shelf)
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

`M-x customize-group RET emacs-reader` 可调整阅读宽度、书架字体和颜色。
