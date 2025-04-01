import { QuartzComponent, QuartzComponentConstructor, QuartzComponentProps } from "./types"
import { classNames } from "../util/lang"

const ArticleTitle: QuartzComponent = ({ fileData, displayClass }: QuartzComponentProps) => {
  const title = fileData.frontmatter?.title
  const subtitle = fileData.frontmatter?.subtitle
  if (title) {
    return (
      <div class={classNames(displayClass, "article-title-container")}>
        <h1 class={classNames(displayClass, "article-title")}>{title}</h1>
        {subtitle && <h2 class={classNames(displayClass, "article-subtitle")}>{subtitle}</h2>}
      </div>
    )
  } else {
    return null
  }
}

ArticleTitle.css = `
.article-title-container {
  margin: 2rem 0 0 0;
}

.article-title {
  margin: 0;
}

.article-subtitle {
  margin: 0.2rem 0 0 0;
  font-size: 1.1rem;
  color: var(--secondary);
  font-weight: normal;
  opacity: 0.8;
}
`

export default (() => ArticleTitle) satisfies QuartzComponentConstructor
