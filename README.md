# emacs-reader

Emacs 原生阅读客户端。书架、已导入的文件和章节进度保存在本机。启动和阅读本地书籍无需 NAS、Reader3 账号或 API 服务。在线书源由 Emacs 直接访问。

## 从本分支加载

```elisp
(add-to-list 'load-path "~/sandbox/emacs-reader") ; 改为本地检出目录
(require 'emacs-reader)
(global-set-key (kbd "C-c r") #'emacs-reader-shelf)
```

运行 `M-x emacs-reader-shelf`，按 `i` 导入单个文件，按 `I` 选择目录并递归导入，按 `RET` 阅读。目录导入期间可在书架按 `C-c C-k`（或运行 `M-x emacs-reader-cancel-directory-import`）取消；扫描阶段立即停止，正在处理的单本书完成后不再导入后续文件，已导入的书保留。导入按文件内容的 SHA-256 去重；文件未变化时复用书库内的摘要索引，首次检查或文件变化后仍需计算。索引可删除，之后会自动重建。支持 EPUB、TXT、PDF、MOBI、Markdown（`.md` / `.markdown`）和 Org（`.org`）。EPUB 需要 Python 3 和带 libxml 的 Emacs；PDF 需要 `pdftotext` 或 `mutool`；MOBI 需要 Calibre 的 `ebook-convert`。带文字层的 PDF 按页提取正文；没有文字层的扫描 PDF 可入架并调用 EmuReader 显示原页面，文字朗读和翻译仍需 OCR。默认数据目录是 `user-emacs-directory` 下的 `emacs-reader/`，可通过 `emacs-reader-directory` 更改。备份或迁移时复制整个目录即可。书籍文件目录可以通过 `emacs-reader-books-directory` 单独设置，例如 `(setq emacs-reader-books-directory "~/Books/Shared/")`；`nil` 表示书库目录下的 `books/`。设置独立目录不会自动移动旧书：原 `books/` 中已登记的书继续从原书库读取，新导入的书写入新目录。之后如果再次更换共享目录，需要先把原共享目录内容复制到新目录；这些书的记录会跟随当前共享目录解析。将已有文件放进共享目录后，可按 `I` 选择该目录登记，源文件无需复制。本地书架与旧 NAS 书架分别保存；要访问旧书架，请将 `emacs-reader-backend` 设为 `reader3`，使用原有服务器配置运行 `M-x emacs-reader-login`。

## 管理书源

在书架按 `S`（`M-x emacs-reader-sources`）打开书源列表。`n` / `p` 选择一行，`a` 添加，`e` 编辑，`g` 刷新，`?` 查看操作。编辑时在 JSON buffer 中修改名称、地址或规则，`C-c C-c` 保存，`C-c C-k` 取消。

按 `/` 输入关键词实时筛选列表；多个词用空格分隔，每个词都要出现在该行，允许分布在不同列。例如 `legado 403` 同时匹配类型和失败原因。已启用 Orderless 拼音匹配的配置也可输入 `chaoshi` 筛选「超时」。`RET` 保留筛选，`C-g` 恢复输入前的筛选，`C-c C-l` 清除筛选。按 `M`（大写）勾选所有可见书源，然后按 `t` 检测，或按 `d`、`x` 确认删除。筛选期间 `t`、`d`、`x` 的批量操作只作用于可见标记项；`T` 仍检测全部书源。

按 `m` 勾选多个书源（行首 `*`），`t` 检测勾选项；没有勾选时检测光标处的书源。`T` 检测全部，`c` 取消检测。检测使用实际搜索并检查首页，默认关键词为「三体」；用 `C-u t` 或 `C-u T` 可更换关键词。列表的「状态」「原因」列会区分 HTTP 错误、超时、规则不支持，以及可浏览但没有搜索入口的 OPDS 目录。Legado 检测同时显示首页返回数，以及书名或作者包含完整关键词的条数，便于识别站点的模糊匹配或推荐列表。首页零条时可换词复测。按 `d` 将勾选项标为待删（行首 `D`）；没有勾选时标记当前行并移到下一行。`u` 撤销当前行标记，`x` 汇总待删项，确认后统一删除。

按 `E`（`M-x emacs-reader-sources-export`）选择全部类型或指定类型，再选择本地输出目录。每个有书源的类型生成一个 ZIP，同一批使用相同的本地时间戳（年月日、时分秒、三位毫秒），例如 `emacs-reader-booksource-legado-backup-20261124181323456.zip`。导出包含所选类型的全部已登记书源，包括筛选隐藏的书源。每包的 `records/` 保留完整注册记录，解压后可用 `a j` 重新导入单个 OPDS 或 JSON 记录；Legado 包另附可用 `a f` 批量导入的 `legado.json`。ZIP 未加密，分享前请检查自定义请求头中是否有私人凭据。

| 想添加的书源 | 在书源列表中操作 | 在书架中使用 |
|---|---|---|
| 文渊阁、Open Library | `a p` 选择示例配置。已添加的文渊阁可直接编辑。 | `s` 搜索。文渊阁也支持 `o` 浏览 OPDS 目录。Open Library 搜索完整书目，支持查看原站详情；有公开下载的版本可继续选取 EPUB / TXT。 |
| Project Gutenberg（古登堡计划） | `a p` 选择 Project Gutenberg。 | `s` 按书名或作者搜索官方目录，选中 EPUB 后按 `RET` 试读。 |
| 自定义 OPDS 1.x / 2.0 | `a o` 输入名称和目录 URL。 | `s` 使用目录公布的搜索入口，`o` 浏览分类。用 `TAB` 选择链接，`RET` 打开，`b` 返回。 |
| Legado | `a f` 从 JSON 文件导入，`a u` 从订阅 URL 导入。支持单源和数组。 | `s` 搜索，`RET` 试读，`a` 将选中的书加入本地书架。 |
| 自定义 JSON 目录 | `a j` 导入完整目录配置。可参考仓库 `examples/` 中的 Open Library 示例。 | `s` 搜索，按配置逐级选择版本和下载文件。 |

例如，一个接口返回 `{"books":[{"title":"书名","download":"/files/book.epub"}]}` 时，可以保存下面这份配置，再用 `a j` 导入。`{query}` 会替换为编码后的关键词，`{download}` 会相对于实际响应地址解析。随仓库分发的完整示例位于 `examples/`；从本地加载时请保留此目录。

```json
{
  "type": "json",
  "name": "我的书库",
  "url": "https://books.example/search?q={query}",
  "definition": {"rows": "books", "label": "title", "href": "{download}"}
}
```

JSON 目录也支持书目详情页。把 `recordHref` 设为详情链接字段（例如 `{key}`），缺少下载链接的作品也会保留，按 `RET` 可在原站查看。Open Library 使用这种规则搜索全部书目，公开下载入口按作品的可用版本单独显示。

搜索只列出已注册且启用的书源。文渊阁和 Open Library 都是可选配置，删除后可通过 `a p` 再次添加。删除书源会保留书架和已下载的文件。尚需联网加载章节的 Legado 书籍，可以在重新添加对应书源后继续阅读。

Legado 支持 HTML/CSS 选择器、tag/class/id 元素链、索引与排除、基础 JSONPath，以及目录和正文分页。搜索 URL 后的 GET/POST 选项和静态请求头可使用单引号或双引号；例如 `{'method':'POST','charset':'gbk','body':'searchkey={{key}}'}` 会按 GBK 编码中文关键词。字段规则支持常见 `##` 正则清洗和 `result.match(/…/)[n]` 捕获。导入时标记为 `enabled: false` 的书源不会出现在搜索列表中。依赖通用 JavaScript、WebView 登录验证或 XPath 的规则目前无法在本地解析。在线搜索与抓取 Legado 章节需要联网；已导入的 EPUB、TXT 可断网阅读。

### 研辞问典

已导入使用 LightRead 公开书目索引的「研辞问典」后，在书架按 `s`，可输入书名或作者，例如 `西游记`、`杨景贤`。首次搜索下载索引，之后在本地进行繁简匹配。选中作品按 `RET` 试读，`c` 看目录，`[` / `]` 切换章节，`a` 加入书架。古籍和中医书目从整本页面分章，四书五经和蒙学按原站目录逐章读取。

原站要求网页验证时，在阅读器按 `v` 打开当前书页，在浏览器完成验证并保存显示正文的 HTML，回到原阅读器按 `i` 选择文件即可继续。也可以按 `e` 试读已有 EPUB：macOS 会列出 LightRead 本地 EPUB 的书名，也可自行选文件。按 `g` 重试原站。

## 搜索范围

在本地书架按 `s` 输入书名或作者。书源选择框支持以下操作。

| 想搜索的范围 | 按键 |
|---|---|
| 上次选的书源 | 直接按 `RET`，默认选中 `【按照上次选的源】`。选择保存在当前书库，重启 Emacs 后仍生效。首次使用时搜索所有已启用书源。 |
| 所有已启用书源 | 选择 `【全部】`，之后复用此范围时也会包含新启用的书源。 |
| 一种类型 | 在空输入处按 `l`（Legado）、`o`（OPDS）或 `s`（JSON，包括 Open Library），再选 `【全部】`。空输入时按 `DEL` 返回完整列表。 |
| 一个书源 | 移到该行按 `RET`。可输入名称定位；如需输入 `l`、`o`、`s` 作为首字母，可先按 `C-q`。 |
| 几个书源 | 移到书源行按 `C-SPC` 勾选，再按一次取消。若系统占用了 `Ctrl` + 空格，可用 `C-@`。可跨类型继续勾选，最后按 `RET` 搜索已勾选的书源。 |

单源和聚合搜索会在书名前标出已知的收费情况。宜搜明确标记收费的书显示 `[含收费章节]`；查看目录后，若免费章和收费章并存，会更新为 `[部分收费]`。只有书源提供收费章节规则、但尚不清楚这本书是否收费时，显示 `[可能含收费章节]`。没有标识表示收费情况未知。

搜索结果也支持 `/` 按收费标识、书名、作者、来源等显示内容筛选，`C-c C-l` 恢复全部结果。筛选只改变当前视图，后台返回的新结果也会按当前条件显示。

搜索期间，结果缓冲区的 mode-line 显示转动的「搜索中」。全部书源和自动分页请求结束后，动效消失。多源搜索会逐步把结果放入同一张表，每行显示来源和可执行的操作。`RET` 试读章节书、打开书目或版本目录。对 EPUB/TXT 下载项，`RET` 取临时副本试读；按 `a` 并确认后才保存到书架。Open Library 等 JSON 目录和支持 `next` 的 OPDS 书源会自动加载后续搜索页，逐页追加到当前结果列表，保留光标位置。`m` 可继续搜索有下一页的 Legado 书源。失败的源单独列出，可在该行按 `RET` 单独重试，或按 `g` 重试所有失败项。

下载 EPUB/TXT 后，目录和章节在后台解析。等待时可以继续编辑或切换窗口；若已经离开下载页，试读会在后台打开，可用 `C-x b` 切回对应的 `*Reader: 书名*`。

## 使用

`M-x emacs-reader-shelf` 打开书架，选择一本书按 `RET` 阅读。书架、正文和目录统一在当前 frame 的 `emacs-reader` tab 中打开；首次自动创建，之后复用，不改变其他 tab 的窗口布局。顶部显示章节名和全书进度（按章节序号估算，非字数进度）。

在书架、正文或目录按 `q` 会关闭（kill）当前 buffer，而非仅隐藏；关闭正文时也会停止朗读并清理音频资源。tab 本身保留。

| 界面 | 按键 | 操作 |
|---|---|---|
| 书架 | `n` / `p` | 下一本 / 上一本 |
| 书架 | `g` / `?` | 刷新书架 / 显示操作菜单 |
| 书架 | `S` | 管理书源 |
| 书架 | `d` | 确认后删除光标所在的书籍，清理对应书库副本 |
| 书架 | `s` | 搜索 Legado、OPDS 书源或 Open Library |
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

删除本地书籍会移除书架记录和章节进度。对于由阅读器复制管理的本地文件，最后一条引用被移除时也会删除书库中的副本；导入前的原文件保留。已在书籍目录内的手动放置文件会直接登记，无需复制，删除书架记录时保留该文件。设置独立共享目录后，删除书架记录也会保留共享目录中的文件。Reader3 书架继续使用服务器端管理。

### 双语阅读与译稿

在正文按 `t` 开启或关闭本书的双语阅读，按 `T` 选择英语 → 中文（默认）或中文 → 英语。每行原文下紧跟对应译文。原文沿用阅读窗口的宽度、字体和自然换行位置。译文按自身长度显示，较长时自然折行。改变窗口宽度或字号后，若原文分行改变，会按新分行读取或生成对应译稿，旧版仍可在译稿库打开。可用 `M-x customize-face RET emacs-reader-translate-view-face` 调整译文颜色或字体。新书默认关闭；重新打开曾开启的书会恢复其翻译方向和双语显示。

开启后先翻当前章，每次最多请求 4 行。每批返回后立刻在对应原文下显示译文，页眉显示「已译 X/Y 行」。中断后再次打开会接着已保存的批次翻译。整章完成后自动保存双语 Org 译稿，再依次处理后续章节。已有译稿直接读取；新译文使用当前 gptel 的后端和模型，只发送待译文本。默认预翻下一章，设为 `0` 仅翻当前章。

```elisp
(setq emacs-reader-translation-prefetch-chapters 1)
;; 可选，将双语文档集中保存到自己的 Org 目录
;; (setq emacs-reader-translation-directory "~/org/书籍译稿/")
```

**译稿按书、章和翻译方向保存为独立的 UTF-8 Org 文档**，默认在当前书库的 `translations/` 下，每份同时包含原文和逐行译文。分行与旧段落不同时会另存逐行版，已有段落稿及其校订保留在译稿库。按 `C-c C-t` 打开本章译稿；在译文段落内校订并保存后，当前阅读页会更新。`M-x emacs-reader-translation-library` 浏览整个译稿库。关闭翻译会保留文档；可以独立备份、迁移或用 Org 导出。原文变化会生成另一版译稿，保留旧版和手工校订。

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

`emacs-reader-tts-rate` 设置默认语速，默认 `1.0`。阅读页按 `>` 加快、`<` 减慢，每次 0.1 倍，范围 0.5～2.0；朗读中调整会从当前段重新朗读并重新生成预取音频。页面顶部在朗读时显示当前语速，调整只作用于当前阅读缓冲区。如服务需要 Bearer token，设置 `emacs-reader-tts-api-key-env` 为 Emacs 进程可读的环境变量名（不是密钥本身）；仅 HTTPS 或本机回环 HTTP 可以发送 token。在线合成会把正文发送给该服务，上面的 HTTP 地址不加密；敏感内容请使用本地服务。

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
