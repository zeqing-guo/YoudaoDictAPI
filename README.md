# YoudaoDictAPI
Unofficial Youdao Dictionary API

有道词典的非官方API

# Usage

```scala
// 获得词语的英式和美式发音
dict.word("naive").uk
dict.word("naive").us

// 获得词语的基本释义、网络释义、专业释义和英英释义
dict.word("naive").simpleDefinition
dict.word("naive").networkDefinition
dict.word("naive").glossaryDefinition
dict.word("naive").eeDefinition

// 获得《21世纪大汉英词典》和《柯林斯英汉双解大辞典》的词条
dict.word("naive").authDict
dict.word("naive").collinsDict

// 获得词组短语、近义词、同根词
dict.word("naive").groups
dict.word("naive").synonyms
dict.word("naive").relWords

// 获得双语例句和权威例句
dict.word("naive").biSentences
dict.word("naive").authSentences
```

# Return Type and Exceptions

返回类型为`String`。

如果所查词汇不存在或者缺少相关条目（比如naive的词典页面没有词组短语这个条目）将会返回`""`。

如果网络链接超时将会返回`"Connection timed out"`。

