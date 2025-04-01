import { pathToRoot } from "../util/path"
import { QuartzComponent, QuartzComponentConstructor, QuartzComponentProps } from "./types"
import { classNames } from "../util/lang"
import { i18n } from "../i18n"

const PageTitle: QuartzComponent = ({ fileData, cfg, displayClass }: QuartzComponentProps) => {
  const title = cfg?.pageTitle ?? i18n(cfg.locale).propertyDefaults.title
  const baseDir = pathToRoot(fileData.slug!)
  
  // Fish Shell Tema Renkleri
  const colors = [
    "#c5c8c6", // Açık Gri (Varsayılan Metin)
    "#81a2be", // Mavi
    "#b294bb", // Mor
    "#8abeb7", // Camgöbeği
    "#b5bd68", // Yeşil
    "#de935f", // Turuncu
    "#cc6666", // Kırmızı
    "#f0c674"  // Sarı
  ];

  return (
    <h2 class={classNames(displayClass, "page-title")}>  
      {title.split(" ").map((word, index) => (
        <a 
          key={index} 
          href={baseDir} 
          className="word-link"
          style={{ color: colors[index % colors.length], marginRight: "0.5rem" }}
        >
          {word}
        </a>
      ))}
    </h2>
  )
}

PageTitle.css = `
.page-title {
  font-size: 1.20rem;
  margin: 0;
  font-family: var(--titleFont);
}

.word-link {
  transition: transform 0.3s, color 0.3s;
}

.word-link:hover {
  transform: scale(1.1);
  color: #81a2be; /* Fish shell'in mavi tonu */
}
`

export default (() => PageTitle) satisfies QuartzComponentConstructor
