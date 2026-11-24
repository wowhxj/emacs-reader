# emacs-reader

Emacs 原生阅读客户端。书架、已导入的文件和章节进度保存在本机。启动和阅读本地书籍无需 NAS、Reader3 账号或 API 服务。在线书源由 Emacs 直接访问。

## 从本分支加载

```elisp
(add-to-list 'load-path "~/sandbox/emacs-reader") ; 改为本地检出目录
(require 'emacs-reader)
(global-set-key (kbd "C-c r") #'emacs-reader-shelf)
```

运行 `M-x emacs-reader-shelf`，按 `i` 导入 TXT 或 EPUB，按 `RET` 阅读。EPUB 需要 Python 3 和带 libxml 的 Emacs。默认数据目录是 `user-emacs-directory` 下的 `emacs-reader/`，可通过 `emacs-reader-directory` 更改。备份或迁移时复制整个目录即可。本地书架与旧 NAS 书架分别保存；要访问旧书架，请将 `emacs-reader-backend` 设为 `reader3`，使用原有服务器配置运行 `M-x emacs-reader-login`。

## 管理书源

在书架按 `S`（`M-x emacs-reader-sources`）打开书源列表。`n` / `p` 选择一行，`a` 添加，`e` 编辑，`g` 刷新，`?` 查看操作。编辑时在 JSON buffer 中修改名称、地址或规则，`C-c C-c` 保存，`C-c C-k` 取消。

按 `/` 输入关键词实时筛选列表；多个词用空格分隔，每个词都要出现在该行，允许分布在不同列。例如 `legado 403` 同时匹配类型和失败原因。已启用 Orderless 拼音匹配的配置也可输入 `chaoshi` 筛选「超时」。`RET` 保留筛选，`C-g` 恢复输入前的筛选，`C-c C-l` 清除筛选。按 `M`（大写）勾选所有可见书源，然后按 `t` 检测，或按 `d`、`x` 确认删除。筛选期间 `t`、`d`、`x` 的批量操作只作用于可见标记项；`T` 仍检测全部书源。

按 `m` 勾选多个书源（行首 `*`），`t` 检测勾选项；没有勾选时检测光标处的书源。`T` 检测全部，`c` 取消检测。检测使用实际搜索并检查首页，默认关键词为「三体」；用 `C-u t` 或 `C-u T` 可更换关键词。列表的「状态」「原因」列会区分 HTTP 错误、超时、规则不支持，以及可浏览但没有搜索入口的 OPDS 目录。Legado 检测同时显示首页返回数，以及书名或作者包含完整关键词的条数，便于识别站点的模糊匹配或推荐列表。首页零条时可换词复测。按 `d` 将勾选项标为待删（行首 `D`）；没有勾选时标记当前行并移到下一行。`u` 撤销当前行标记，`x` 汇总待删项，确认后统一删除。

| 想添加的书源 | 在书源列表中操作 | 在书架中使用 |
|---|---|---|
| 文渊阁、Open Library | `a p` 选择示例配置。已添加的文渊阁可直接编辑。 | `s` 搜索。文渊阁也支持 `o` 浏览 OPDS 目录。Open Library 搜索完整书目，支持查看原站详情；有公开下载的版本可继续选取 EPUB / TXT。 |
| 自定义 OPDS 1.x / 2.0 | `a o` 输入名称和目录 URL。 | `s` 使用目录公布的搜索入口，`o` 浏览分类。用 `TAB` 选择链接，`RET` 打开，`b` 返回。 |
| Legado | `a f` 从 JSON 文件导入，`a u` 从订阅 URL 导入。支持单源和数组。 | `s` 搜索，`RET` 试读，`a` 将选中的书加入本地书架。 |
| 自定义 JSON 目录 | `a j` 导入完整目录配置。可参考仓库 `examples/` 中的 Open Library 示例。 | `s` 搜索，按配置逐级选择版本和下载文件。 |

例如，一个接口返回 `{"books":[{"title":"书名","download":"/files/book.epub"}]}` 时，可以保存下面这份配置，再用 `a j` 导入。`{query}` 会替换为编码后的关键词，`{download}` 会相对于实际响应地址解析。随仓库分发的两份完整示例位于 `examples/`；从本地加载时请保留此目录。

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
| 所有已启用书源 | 直接按 `RET`，默认选中 `【全部】`。 |
| 一种类型 | 在空输入处按 `l`（Legado）、`o`（OPDS）或 `s`（JSON，包括 Open Library），再选 `【全部】`。空输入时按 `DEL` 返回完整列表。 |
| 一个书源 | 移到该行按 `RET`。可输入名称定位；如需输入 `l`、`o`、`s` 作为首字母，可先按 `C-q`。 |
| 几个书源 | 移到书源行按 `C-SPC` 勾选，再按一次取消。若系统占用了 `Ctrl` + 空格，可用 `C-@`。可跨类型继续勾选，最后按 `RET` 搜索已勾选的书源。 |

搜索结果也支持 `/` 按书名、作者、来源等显示内容筛选，`C-c C-l` 恢复全部结果。筛选只改变当前视图，后台返回的新结果也会按当前条件显示。

搜索期间，结果缓冲区的 mode-line 显示转动的「搜索中」。全部书源和自动分页请求结束后，动效消失。多源搜索会逐步把结果放入同一张表，每行显示来源和可执行的操作。`RET` 试读章节书、打开书目或版本目录。对 EPUB/TXT 下载项，`RET` 取临时副本试读；按 `a` 并确认后才保存到书架。Open Library 等 JSON 目录和支持 `next` 的 OPDS 书源会自动加载后续搜索页，逐页追加到当前结果列表，保留光标位置。`m` 可继续搜索有下一页的 Legado 书源。失败的源单独列出，可在该行按 `RET` 单独重试，或按 `g` 重试所有失败项。

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

删除本地书籍会移除书架记录和章节进度。若为 EPUB 或 TXT，最后一条引用被移除时也会删除书库中的文件副本。导入前的原文件保留。Reader3 书架继续使用服务器端管理。

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
