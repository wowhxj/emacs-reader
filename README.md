# emacs-reader

Emacs 原生阅读客户端。书架、已导入的文件和章节进度保存在本机。启动和阅读本地书籍无需 NAS、Reader3 账号或 API 服务。在线书源由 Emacs 直接访问。

## 从本分支加载

```elisp
(add-to-list 'load-path "~/sandbox/emacs-reader") ; 改为本地检出目录
(require 'emacs-reader)
(global-set-key (kbd "C-c r") #'emacs-reader-shelf)
```

运行 `M-x emacs-reader-shelf`，按 `i` 导入 TXT 或 EPUB，按 `RET` 阅读。EPUB 需要 Python 3 和带 libxml 的 Emacs。默认数据目录是 `user-emacs-directory` 下的 `emacs-reader/`，可通过 `emacs-reader-directory` 更改。备份或迁移时复制整个目录即可。本地书架与旧 NAS 书架分别保存；要访问旧书架，请将 `emacs-reader-backend` 设为 `reader3`，使用原有服务器配置运行 `M-x emacs-reader-login`。

## 添加书源

| 来源 | 添加与阅读 |
|---|---|
| Legado | `M-x emacs-reader-import-legado` 导入 JSON 文件，支持单个书源或书源数组。用 `C-u M-x emacs-reader-import-legado` 输入订阅 URL。相同书源地址再次导入会更新规则。书架按 `s` 输入关键词并选择书源，结果按 `RET` 试读、按 `a` 加入本地书架。 |
| OPDS | `M-x emacs-reader-add-opds` 保存目录名称和 URL。书架按 `o` 打开目录，支持 OPDS 1.x Atom 和 2.0 JSON。用 `TAB` 选择分类、搜索、下一页或下载链接，`RET` 打开，`b` 返回。直接获取的 EPUB、TXT 下载到本地书架。 |
| Open Library | 书架按 `s` 选择内置的 Open Library，也可运行 `M-x emacs-reader-openlibrary-search`。在结果中选择公开版本和 EPUB / TXT 文件。需要借阅、登录或 DRM 的版本不提供本地下载。 |

Legado 支持声明式 HTML/CSS 选择器、原生 tag/class/id 规则、基础 JSONPath、GET/POST、静态请求头，以及目录和正文分页。导入时标记为 `enabled: false` 的书源不会出现在搜索列表中。依赖 JavaScript、WebView、XPath 或复杂脚本的书源需要额外引擎。在线搜索与抓取 Legado 章节需要联网；已导入的 EPUB、TXT 可断网阅读。

## 使用

`M-x emacs-reader-shelf` 打开书架，选择一本书按 `RET` 阅读。书架、正文和目录统一在当前 frame 的 `emacs-reader` tab 中打开；首次自动创建，之后复用，不改变其他 tab 的窗口布局。顶部显示章节名和全书进度（按章节序号估算，非字数进度）。

在书架、正文或目录按 `q` 会关闭（kill）当前 buffer，而非仅隐藏；关闭正文时也会停止朗读并清理音频资源。tab 本身保留。

| 界面 | 按键 | 操作 |
|---|---|---|
| 书架 | `n` / `p` | 下一本 / 上一本 |
| 书架 | `g` | 刷新 |
| 书架 | `s` | 搜索 Legado 书源或 Open Library |
| 搜索 | `n` / `p` | 下一条 / 上一条结果 |
| 搜索 | `RET` | 试读，不加入书架 |
| 搜索 | `a` | 确认后加入书架 |
| 搜索 | `m` / `g` | 搜索下一批；失败时重试当前批 |
| 搜索 | `s` | 输入新关键词并选择搜索范围 |
| 阅读 | `a` | 将当前试读书籍加入书架 |
| 阅读 | `j` / `k` | 向下 / 向上滚一行 |
| 阅读 | `d` / `u` | 向下 / 向上翻半屏 |
| 阅读 | `n` / `SPC` | 向下翻整屏；在章末进入下一章章首 |
| 阅读 | `p` | 向上翻整屏；在章首回到上一章末屏 |
| 阅读 | `]` / `[` | 下一章 / 上一章 |
| 阅读 | `c` | 打开目录，`▶ 当前` 标记正在阅读的章节 |
| 阅读 | `C` | 按章节编号跳转 |
| 阅读 | `r` | 开始 / 停止逐段朗读，播放段落高亮 |
| 目录 | `n` / `p` | 下一项 / 上一项 |
| 目录 | `RET` | 跳转选中章节 |
| 全部 | `q` | 关闭当前 buffer |

### 朗读配置

默认 `emacs-reader-tts-url` 为 `nil`，按 `r` 调用 macOS 的 `say` 或本机安装的 `espeak-ng` / `espeak`。可用 `emacs-reader-tts-native-voice` 指定本机已安装的音色，例如 macOS 的 `Tingting`。

如已配置 OpenAI 兼容 TTS 端点，`r` 会继续使用该端点。以下为两种配置示例。

**本地 mac-tts**（按其 README 启动服务）：

```elisp
(setq emacs-reader-tts-url "http://127.0.0.1:8095/v1/audio/speech"
      emacs-reader-tts-model "kokoro-multi-lang-v1_1"
      emacs-reader-tts-voice "zf_001"
      emacs-reader-tts-format 'wav)
```

**在线 zb-tts**（晓晓）：

```elisp
(setq emacs-reader-tts-url "http://remacs.cc:8104/v1/audio/speech"
      emacs-reader-tts-model "tts-1"
      emacs-reader-tts-voice "zh-CN-XiaoxiaoNeural"
      emacs-reader-tts-format 'mp3)
```

`emacs-reader-tts-rate` 调节语速，默认 `1.0`。如服务需要 Bearer token，设置 `emacs-reader-tts-api-key-env` 为 Emacs 进程可读的环境变量名（不是密钥本身）；仅 HTTPS 或本机回环 HTTP 可以发送 token。在线合成会把正文发送给该服务，上面的 HTTP 地址不加密；敏感内容请使用本地服务。

端点返回的音频需本机有 `afplay`（macOS 自带）、`mpv` 或 `ffplay`。`r` 从光标所在段落开始朗读，播放时高亮并预取下一段。本章读完后自动加载下一章，从首段继续，直到全书读完；下一章加载失败时会停止。等待自动翻章时也可按 `r` 停止朗读；手动切章或关闭缓冲区会停止并清理预取。朗读不同步服务器进度。

WSL2 + WSLg 可安装 Linux 侧 `mpv` 播放。没有 Linux 音频输出时，可在 Windows 安装 `mpv` 或 `ffplay` 并加入 PATH，WSL 中确认 `command -v mpv.exe` 或 `command -v ffplay.exe` 能找到；客户端会用 `wslpath` 转换音频路径。若需要强制使用 Windows 播放器：

```elisp
(setq emacs-reader-tts-player-command '("mpv.exe" "--no-video" "--really-quiet"))
```

## 开发验证

```sh
emacs -Q --batch -L . --eval '(setq load-prefer-newer t)' \
  --eval '(mapc (lambda (f) (load f nil t)) (directory-files "test" t "-test\\.el$"))' \
  -f ert-run-tests-batch-and-exit
```

`M-x customize-group RET emacs-reader` 可调整阅读宽度、书架字体和颜色。
