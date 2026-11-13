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

`M-x emacs-reader-shelf` 打开书架，选择一本书按 `RET` 阅读。

| 界面 | 按键 | 操作 |
|---|---|---|
| 书架 | `n` / `p` | 下一本 / 上一本 |
| 书架 | `g` | 刷新 |
| 阅读 | `j` / `k` | 向下 / 向上滚一行 |
| 阅读 | `d` / `u` | 向下 / 向上翻半屏 |
| 阅读 | `n` / `p` | 向下 / 向上翻整屏 |
| 阅读 | `SPC` | 向下翻整屏 |
| 阅读 | `]` / `[` | 下一章 / 上一章 |
| 阅读 | `c` | 按章节编号跳转 |

`M-x customize-group RET emacs-reader` 可调整阅读宽度、书架字体和颜色。
