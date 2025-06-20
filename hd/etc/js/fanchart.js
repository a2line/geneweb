const root = document.documentElement;
const fanchart = document.getElementById("fanchart");
const pixel = document.getElementById("pixel").getContext("2d", { willReadFrequently: true });
var sheet;
for (var i in document.styleSheets) {
  if (document.styleSheets[i].title == "fc-auto") {
    sheet = document.styleSheets[i];
    break;
  }
}
var standard, standard_width;
var center_x, center_y, svg_w, svg_h;
var max_gen_loaded; // Génération max disponible en "mémoire"
var max_gen, max_r;
var lieux = {};      // Objet principal des lieux : clé = nom du lieu, valeur = données
var lieux_a = [];    // Array des lieux pour le tri et l'itération
var sortMode = "frequency";
var showEvents = false;
var has_bi = false, has_ba = false, has_ma = false, has_de = false, has_bu = false;
var svg_viewbox_x = 0, svg_viewbox_y = 0, svg_viewbox_w = 0, svg_viewbox_h = 0;

// ====== Configuration =======
const CONFIG = {
  security: 0.95,
  zoom_factor: 1.25,
  default_angle: 220,
  available_angles: [180, 220, 359],
  a_r: [50, 50, 50, 50, 80, 70, 100, 150, 130, 90],
  a_m: ["S1", "C3", "C3", "C3", "R3", "R3", "R2", "R1", "R1", "R1"],
  marriage_length_thresholds: [4, 14, 24, 34, 44, 54],
  text_reduction_factor: 0.9,
  svg_margin: 5
};

let isCircularMode = false;
let renderTarget = null; // (null = fanchart direct)
let current_angle = CONFIG.default_angle;


// Configuration des événements
const EVENT_CONFIG = {
  eventOrder: ['birth', 'baptism', 'marriage', 'death', 'burial'],
  eventToClass: { 'birth': 'n', 'baptism': 'b', 'marriage': 'm', 'death': 'd', 'burial': 's' },
  eventToLabel: { 'birth': 'N', 'baptism': 'B', 'marriage': 'M', 'death': 'D', 'burial': 'S' }
};

const Events = {
  get types() { return EVENT_CONFIG.eventOrder; },
  cssClass: (type) => EVENT_CONFIG.eventToClass[type],
  label: (type) => EVENT_CONFIG.eventToLabel[type],
  count: (type) => type + '_count',
  place: (type) => type + '_place',
  svgPrefix: (type) => type.substring(0, 2),
  flagProp: (type) => 'has_' + Events.svgPrefix(type),
  findByClass: (cssClass) => EVENT_CONFIG.eventOrder.find(type => Events.cssClass(type) === cssClass),
  findBySvgPrefix: (prefix) => EVENT_CONFIG.eventOrder.find(type => Events.svgPrefix(type) === prefix),
  isValid: (type) => EVENT_CONFIG.eventOrder.includes(type),
  translate: (type, count = 1) => {
    const translationKey = count > 1 ? type + 's' : type;
    return window.FC_TRANSLATIONS?.[translationKey] || type;
  }
};

// Utilitaires
const Utils = {
  calculateAgeCategory: function(age) {
    const boundaries = [30, 45, 60, 75, 90, 105, Infinity];
    const category = boundaries.findIndex(boundary => age < boundary);
    return Math.min(category, 6);
  },

  deathAgeClass: function(age) {
    return "DA" + this.calculateAgeCategory(age);
  },

  marriageLengthClass: function(length) {
    const years = parseInt(length);
    if (isNaN(years) || years < 0) return "";
    const marriageThresholds = [4, 14, 24, 34, 44, 54];
    const index = marriageThresholds.findIndex(threshold => years <= threshold);
    return index === -1 ? "DAM6" : `DAM${index}`;
  }
};

/**
 * HighlightStateManager - Gestionnaire d'état centralisé pour tous les surlignages
 *
 * Ce module maintient l'état global de tous les surlignages actifs et coordonne
 * les transitions entre les différents états. Il garantit qu'un seul type de
 * surlignage peut être actif à la fois et gère le nettoyage approprié.
 */
const HighlightStateManager = {
  // État complet du système de surlignage
  state: {
    // Type de surlignage actif
    activeHighlight: {
      source: null,      // 'sector', 'place', 'event-total', 'age-legend', null
      sourceElement: null, // L'élément DOM qui a déclenché le surlignage
      type: null,        // 'person', 'marriage', 'place', 'event', 'age'
      data: null         // Données associées au surlignage
    },

    // Collections d'éléments modifiés pour restauration
    modifiedElements: {
      svg: new Map(),       // Éléments SVG modifiés
      dom: new Map(),       // Éléments DOM (liste) modifiés
      classes: new Map()    // Classes ajoutées/supprimées
    },

    // État des modes visuels
    visualModes: {
      placeColorActive: false,
      deathAgeActive: false,
      showEvents: false
    },

    // Cache des données pour optimisation
    cache: {
      lastHighlight: null,
      preventRedundant: true
    }
  },

  /**
   * Initialise un nouveau surlignage en nettoyant l'état précédent
   * @param {string} source - Source du surlignage ('sector', 'place', etc.)
   * @param {string} type - Type spécifique ('person', 'marriage', etc.)
   * @param {*} data - Données associées
   * @param {HTMLElement} sourceElement - Élément déclencheur
   * @returns {boolean} true si le surlignage peut procéder
   */
  startHighlight(source, type, data, sourceElement = null) {
    // Vérifier si c'est le même surlignage (optimisation)
    if (this.state.cache.preventRedundant && this.isSameHighlight(source, type, data)) {
      return false;
    }

    // Nettoyer l'état précédent
    this.clearAllHighlights();

    // Enregistrer le nouvel état
    this.state.activeHighlight = {
      source,
      sourceElement,
      type,
      data
    };

    // Mettre en cache pour détecter les répétitions
    this.state.cache.lastHighlight = { source, type, data };

    // Logger pour debug
    console.log(`🎯 Surlignage démarré: ${source}/${type}`, data);

    return true;
  },

  /**
   * Vérifie si le surlignage demandé est identique au précédent
   */
  isSameHighlight(source, type, data) {
    const last = this.state.cache.lastHighlight;
    if (!last) return false;

    // Comparaison basique pour les types simples
    if (last.source !== source || last.type !== type) return false;

    // Comparaison approfondie selon le type
    if (source === 'sector' && data?.sosa) {
      return last.data?.sosa === data.sosa;
    } else if (source === 'place' && data?.placeName) {
      return last.data?.placeName === data.placeName;
    } else if (source === 'event-total' && data?.eventType) {
      return last.data?.eventType === data.eventType;
    }

    return false;
  },

  /**
   * Enregistre un élément modifié pour restauration ultérieure
   * @param {string} category - 'svg', 'dom', ou 'classes'
   * @param {Element} element - L'élément modifié
   * @param {Object} originalState - État original à restaurer
   */
  trackModification(category, element, originalState) {
    if (!element) return;

    const collection = this.state.modifiedElements[category];
    if (!collection) {
      console.warn(`Catégorie invalide: ${category}`);
      return;
    }

    // Stocker l'état original si pas déjà tracké
    if (!collection.has(element)) {
      collection.set(element, originalState);
    }
  },

  /**
   * Nettoie complètement tous les surlignages actifs
   */
  clearAllHighlights() {
    // Restaurer les éléments SVG
    this.state.modifiedElements.svg.forEach((originalState, element) => {
      if (originalState.fill !== undefined) {
        element.style.fill = originalState.fill;
      }
      if (originalState.fillOpacity !== undefined) {
        element.style.fillOpacity = originalState.fillOpacity;
      }
      if (originalState.stroke !== undefined) {
        element.style.stroke = originalState.stroke;
      }

      // Nettoyer les data attributes
      ['originalFill', 'originalOpacity', 'highlighted', 'listHighlighted'].forEach(attr => {
        delete element.dataset[attr];
      });
    });

    // Restaurer les éléments DOM
    this.state.modifiedElements.dom.forEach((originalState, element) => {
      if (originalState.innerHTML !== undefined) {
        element.innerHTML = originalState.innerHTML;
      }
      if (originalState.display !== undefined) {
        element.style.display = originalState.display;
      }
    });

    // Restaurer les classes
    this.state.modifiedElements.classes.forEach((operations, element) => {
      operations.added.forEach(className => element.classList.remove(className));
      operations.removed.forEach(className => element.classList.add(className));
    });

    // Vider les collections
    this.state.modifiedElements.svg.clear();
    this.state.modifiedElements.dom.clear();
    this.state.modifiedElements.classes.clear();

    // Réinitialiser l'état actif
    this.state.activeHighlight = {
      source: null,
      sourceElement: null,
      type: null,
      data: null
    };

  },

  /**
   * Met à jour l'état des modes visuels
   */
  updateVisualModes() {
    this.state.visualModes.placeColorActive = document.body.classList.contains('place_color');
    this.state.visualModes.deathAgeActive = document.body.classList.contains('death-age');
    this.state.visualModes.showEvents = document.querySelector('.places-panel')?.classList.contains('show-events') || false;
  },

  /**
   * Vérifie si un type de surlignage est actuellement actif
   */
  isHighlightActive(source = null, type = null) {
    if (!source) {
      return this.state.activeHighlight.source !== null;
    }

    if (!type) {
      return this.state.activeHighlight.source === source;
    }

    return this.state.activeHighlight.source === source &&
           this.state.activeHighlight.type === type;
  },

  /**
   * Obtient les données du surlignage actif
   */
  getActiveHighlight() {
    return { ...this.state.activeHighlight };
  },

  /**
   * Enregistre une opération sur les classes CSS
   */
  trackClassOperation(element, added = [], removed = []) {
    if (!element) return;

    let operations = this.state.modifiedElements.classes.get(element);
    if (!operations) {
      operations = { added: new Set(), removed: new Set() };
      this.state.modifiedElements.classes.set(element, operations);
    }

    // Enregistrer les classes ajoutées
    added.forEach(className => {
      operations.added.add(className);
      // Si la classe était dans removed, l'enlever
      operations.removed.delete(className);
    });

    // Enregistrer les classes supprimées
    removed.forEach(className => {
      operations.removed.add(className);
      // Si la classe était dans added, l'enlever
      operations.added.delete(className);
    });
  },

  /**
   * Interface de débogage pour afficher l'état actuel
   */
  debugState() {
    console.group('📊 État du HighlightStateManager');
    console.log('Surlignage actif:', this.state.activeHighlight);
    console.log('Éléments SVG modifiés:', this.state.modifiedElements.svg.size);
    console.log('Éléments DOM modifiés:', this.state.modifiedElements.dom.size);
    console.log('Classes modifiées:', this.state.modifiedElements.classes.size);
    console.log('Modes visuels:', this.state.visualModes);
    console.groupEnd();
  }
};

/**
 * SVGHighlightHandler - Gestionnaire spécialisé pour les surlignages dans le SVG
 * 
 * Ce module gère exclusivement les modifications visuelles des éléments SVG,
 * incluant les secteurs individuels, les mariages, et les colorisations par lieu/événement.
 */
const SVGHighlightHandler = {
  // Configuration des styles de surlignage
  config: {
    hover: {
      fill: 'lightgrey',
      fillOpacity: '0.3'
    },
    
    eventColors: {
      birth: '#4CAF50',
      baptism: '#66BB6A', 
      marriage: '#E67E22',
      death: '#757575',
      burial: '#9E9E9E',
      multiple: '#2196F3'  // Pour les événements multiples
    },
    
    classes: {
      sectorHover: 'sector-highlighted',
      placeHighlight: 'svg-place-highlight',
      eventHighlight: 'event-highlight-',  // Préfixe pour event-highlight-birth, etc.
      ageHighlight: 'age-highlight',
      backgroundHighlight: 'bg-highlighted'
    }
  },
  
  /**
   * Surligne un secteur individuel au survol
   * @param {SVGElement} sector - Le secteur SVG à surligner
   * @param {Object} person - Les données de la personne
   * @param {string} type - 'person' ou 'marriage'
   */
  highlightSector: function(sector, person, type) {
    if (!sector) return;
    
    // Sauvegarder l'état original
    const originalState = {
      fill: sector.style.fill || '',
      fillOpacity: sector.style.fillOpacity || '',
      stroke: sector.style.stroke || ''
    };
    
    // Enregistrer dans le gestionnaire d'état
    HighlightStateManager.trackModification('svg', sector, originalState);
    
    // Appliquer le style de survol
    sector.style.fill = this.config.hover.fill;
    sector.style.fillOpacity = this.config.hover.fillOpacity;
    sector.classList.add(this.config.classes.sectorHover);
    
    // Traquer l'opération de classe
    HighlightStateManager.trackClassOperation(sector, [this.config.classes.sectorHover]);
    
    // Surligner aussi le secteur de fond s'il existe
    this.highlightBackgroundSector(sector.parentNode, person, type);
  },
  
  /**
   * Surligne le secteur de fond associé
   */
  highlightBackgroundSector: function(group, person, type) {
    if (!group) return;
    
    const bgSector = group.querySelector('.bg');
    if (!bgSector) return;
    
    // Sauvegarder l'état
    const originalState = {
      fill: bgSector.style.fill || ''
    };
    
    HighlightStateManager.trackModification('svg', bgSector, originalState);
    
    // Appliquer le surlignage
    bgSector.style.fill = this.config.hover.fill;
    bgSector.classList.add(this.config.classes.backgroundHighlight);
    
    HighlightStateManager.trackClassOperation(bgSector, [this.config.classes.backgroundHighlight]);
  },
  
  /**
   * Colorise les secteurs SVG selon les lieux
   * @param {string} placeName - Nom du lieu
   * @param {Object} placeData - Données du lieu
   * @param {Array} eventTypes - Types d'événements à coloriser
   */
  highlightByPlace: function(placeName, placeData, eventTypes) {
    if (!placeData || !placeData.c) {
      console.warn('⚠️ Données de lieu invalides:', placeName);
      return;
    }
    
    const placeClass = placeData.c; // Ex: "L0"
    console.log(`🎨 Colorisation pour le lieu ${placeName} (${placeClass})`);
    
    // Si pas d'événements spécifiés, utiliser tous ceux du lieu
    const eventsToHighlight = eventTypes || this.getPlaceEvents(placeData);
    
    eventsToHighlight.forEach(eventType => {
      const svgPrefix = Events.svgPrefix(eventType);
      
      if (!placeData[svgPrefix]) return;
      
      const color = this.config.eventColors[eventType];
      
      if (eventType === 'marriage') {
        // Chercher les secteurs de mariage
        const marriageSectors = document.querySelectorAll(`path.link.ma-${placeClass}`);
        console.log(`  - Mariages trouvés: ${marriageSectors.length}`);
        
        marriageSectors.forEach(sector => {
          this.applyColorHighlight(sector, color, 'marriage');
        });
      } else {
        // Chercher les secteurs individuels
        const selectors = [
          `path.link.${svgPrefix}-${placeClass}`,
          `circle.link.${svgPrefix}-${placeClass}`
        ];
        
        selectors.forEach(selector => {
          const elements = document.querySelectorAll(selector);
          console.log(`  - ${eventType} (${selector}): ${elements.length} éléments`);
          
          elements.forEach(element => {
            this.applyColorHighlight(element, color, eventType);
          });
        });
      }
    });
  },

  /**
   * Trouve tous les individus ayant des événements dans un lieu donné
   * Cette méthode est nécessaire pour implémenter correctement la logique
   * de colorisation selon le nombre d'événements par individu dans un lieu
   * @private
   */
  findIndividualsWithPlaceEvents: function(placeClass, eventTypes) {
    const individuals = new Map(); // Clé: ID groupe, Valeur: {element, events}
    
    // Parcourir tous les groupes d'individus
    const individualGroups = document.querySelectorAll('g[id^="S"]');
    
    individualGroups.forEach(group => {
      const groupId = group.id;
      const eventsInPlace = [];
      
      // Vérifier chaque type d'événement (sauf mariage)
      eventTypes.filter(et => et !== 'marriage').forEach(eventType => {
        const svgPrefix = Events.svgPrefix(eventType);
        
        // Chercher les éléments avec cette classe
        const selector = `path.link.${svgPrefix}-${placeClass}, circle.link.${svgPrefix}-${placeClass}`;
        const elements = group.querySelectorAll(selector);
        
        if (elements.length > 0) {
          eventsInPlace.push({
            type: eventType,
            element: elements[0] // Premier élément interactif trouvé
          });
        }
      });
      
      // Si l'individu a des événements dans ce lieu, l'ajouter à la map
      if (eventsInPlace.length > 0) {
        individuals.set(groupId, {
          elements: eventsInPlace,
          count: eventsInPlace.length
        });
      }
    });
    
    // Convertir en array pour le traitement
    const result = [];
    individuals.forEach((data, groupId) => {
      data.elements.forEach(event => {
        result.push({
          element: event.element,
          eventCount: data.count,
          eventType: event.type,
          groupId: groupId
        });
      });
    });
    
    return result;
  },

  /**
   * Surligne les secteurs de mariage pour un lieu spécifique
   */
  highlightMarriageSectorsForPlace: function(placeClass, color) {
    // Chercher dans tous les groupes de mariage
    const marriageGroups = document.querySelectorAll('g[id^="M"]');
    
    marriageGroups.forEach(group => {
      // Chercher les secteurs de mariage avec cette classe de lieu
      const selector = `path.ma-${placeClass}`;
      const marriageSectors = group.querySelectorAll(selector);
      
      marriageSectors.forEach(sector => {
        if (sector.classList.contains('link') && !sector.classList.contains('bg')) {
          this.applyColorHighlight(sector, color, 'marriage');
        }
      });
    });
  },

  /**
   * Surligne les secteurs individuels pour un lieu et événement spécifiques
   */
  highlightIndividualSectorsForPlace: function(placeClass, svgPrefix, color, eventType) {
    // Chercher dans tous les groupes d'individus
    const individualGroups = document.querySelectorAll('g[id^="S"]');
    
    individualGroups.forEach(group => {
      // Sélecteurs pour path et circle
      const selectors = [
        `path.${svgPrefix}-${placeClass}`,
        `circle.${svgPrefix}-${placeClass}`
      ];
      
      selectors.forEach(selector => {
        const elements = group.querySelectorAll(selector);
        elements.forEach(element => {
          if (element.classList.contains('link') && !element.classList.contains('bg')) {
            // Déterminer la couleur finale
            // Si un seul événement individuel dans ce lieu, utiliser sa couleur
            // Sinon, utiliser la couleur passée en paramètre
            const finalColor = (eventType !== 'marriage') ? 
              (this.config.eventColors[eventType] || color) : color;
            
            this.applyColorHighlight(element, finalColor, eventType);
          }
        });
      });
    });
  },
  
  /**
   * Colorise les secteurs par type d'événement
   * @param {string} eventType - Type d'événement ('birth', 'marriage', etc.)
   */
  highlightByEventType(eventType) {
    if (!Events.isValid(eventType)) return;

    const color = this.config.eventColors[eventType];

    if (eventType === 'marriage') {
      // SPEC: "Pour M (mariages) - CAS SPÉCIAL"
      // "seuls les SECTEURS DE MARIAGE sont colorisés en orange"
      this.highlightMarriageSectorsOnly(color);
    } else {
      // Autres événements : secteurs d'individus uniquement
      this.highlightIndividualsByEvent(eventType, color);
    }
  },
  
  /**
   * Surligne uniquement les secteurs de mariage
   */
  highlightMarriageSectorsOnly: function(color) {
    // Chercher dans tous les groupes de mariage
    const marriageGroups = document.querySelectorAll('g[id^="M"]');
    
    marriageGroups.forEach(group => {
      // Chercher tous les secteurs de mariage avec des lieux
      const marriageSectors = group.querySelectorAll('path.link[class*="ma-L"]');
      
      marriageSectors.forEach(sector => {
        // Vérifier que c'est bien un secteur de mariage avec lieu
        const classMatch = sector.className.baseVal.match(/ma-(L\d+)/);
        if (classMatch && !sector.classList.contains('bg')) {
          this.applyColorHighlight(sector, color, 'marriage');
        }
      });
    });
  },
  
  /**
   * Surligne les secteurs d'individus par événement
   */
  highlightIndividualsByEvent: function(eventType, color) {
    const svgPrefix = Events.svgPrefix(eventType);
    
    // Chercher dans tous les groupes d'individus
    const individualGroups = document.querySelectorAll('g[id^="S"]');
    
    individualGroups.forEach(group => {
      // Chercher les éléments avec la classe d'événement
      const selectors = [
        `path.link[class*="${svgPrefix}-L"]`,
        `circle.link[class*="${svgPrefix}-L"]`
      ];
      
      selectors.forEach(selector => {
        const elements = group.querySelectorAll(selector);
        elements.forEach(element => {
          // Vérifier que c'est bien un secteur avec ce type d'événement
          const classMatch = element.className.baseVal.match(new RegExp(`${svgPrefix}-(L\\d+)`));
          if (classMatch && !element.classList.contains('bg')) {
            this.applyColorHighlight(element, color, eventType);
          }
        });
      });
    });
  },
  
  /**
   * Applique une colorisation à un élément SVG
   */
  applyColorHighlight: function(element, color, eventType) {
    // Sauvegarder l'état original
    const originalState = {
      fill: element.style.fill || '',
      fillOpacity: element.style.fillOpacity || ''
    };
    
    HighlightStateManager.trackModification('svg', element, originalState);
    
    // Appliquer la colorisation
    element.style.fill = color;
    element.style.fillOpacity = '0.5';
    
    // Ajouter la classe spécifique
    const highlightClass = `${this.config.classes.eventHighlight}${eventType}`;
    element.classList.add(highlightClass);
    
    HighlightStateManager.trackClassOperation(element, [highlightClass]);
  },
  
  /**
   * Obtient les événements présents pour un lieu
   */
  getPlaceEvents: function(placeData) {
    return Events.types.filter(eventType => 
      placeData[Events.svgPrefix(eventType)]
    );
  },
  
  /**
   * Gère le surlignage des secteurs liés aux âges au décès
   * @param {string} ageClass - Classe d'âge (DA0-DA6)
   * @param {boolean} highlight - true pour surligner, false pour nettoyer
   */
  highlightAgeGroup: function(ageClass, highlight) {
    // Chercher tous les éléments avec cette classe d'âge
    const elements = document.getElementsByClassName(ageClass);
    
    Array.from(elements).forEach(element => {
      if (highlight) {
        // Sauvegarder l'état
        const originalState = {
          fill: element.style.fill || '',
          fillOpacity: element.style.fillOpacity || ''
        };
        
        HighlightStateManager.trackModification('svg', element, originalState);
        
        // Appliquer le surlignage
        element.classList.add(this.config.classes.ageHighlight);
        element.style.fillOpacity = '0.5';
        
        HighlightStateManager.trackClassOperation(element, [this.config.classes.ageHighlight]);
      }
      // Note: le nettoyage est géré par HighlightStateManager
    });
  },
  
  /**
   * Méthode de débogage pour visualiser les éléments surlignés
   */
  debugHighlightedElements: function() {
    console.group('🎨 Éléments SVG surlignés');
    
    // Secteurs surlignés
    const highlighted = document.querySelectorAll(`.${this.config.classes.sectorHover}`);
    console.log(`Secteurs en survol: ${highlighted.length}`);
    
    // Éléments colorisés par événement
    Events.types.forEach(eventType => {
      const className = `${this.config.classes.eventHighlight}${eventType}`;
      const elements = document.querySelectorAll(`.${className}`);
      if (elements.length > 0) {
        console.log(`${eventType}: ${elements.length} éléments`);
      }
    });
    
    // Éléments d'âge surlignés
    const ageElements = document.querySelectorAll(`.${this.config.classes.ageHighlight}`);
    if (ageElements.length > 0) {
      console.log(`Éléments d'âge: ${ageElements.length}`);
    }
    
    console.groupEnd();
  }
};

/**
 * ListHighlightHandler - Gestionnaire spécialisé pour les surlignages dans la liste des lieux
 * 
 * Ce module gère exclusivement les modifications visuelles dans le panneau des lieux,
 * incluant le surlignage des lignes, l'ajout d'indicateurs d'événements, et la gestion
 * de l'overflow pour les éléments hors de vue.
 */
const ListHighlightHandler = {
  // Configuration des styles et comportements
  config: {
    classes: {
      personMatch: 'person-match',
      grayedOut: 'grayed-out',
      tallRow: 'tall-row',
      expandedName: 'name-expanded'
    },
    
    indicators: {
      maxBeforeBreak: 3,  // Nombre d'indicateurs avant saut de ligne
      containerClass: 'place-indicators',
      indicatorClass: 'indicator',
      lineBreakClass: 'line-break'
    },
    
    overflow: {
      sectionClass: 'overflow-section',
      headerClass: 'overflow-header',
      contentClass: 'overflow-content',
      maxItems: 5,  // Maximum d'éléments dans une section overflow
      animationDelay: 50  // Délai avant affichage overflow (ms)
    }
  },
  
  // État interne
  state: {
    highlightedPlaces: new Set(),
    addedIndicators: new Map(),  // element -> [indicators]
    expandedNames: new Map(),    // element -> originalHTML
    overflowSections: []
  },
  
  /**
   * Surligne un ou plusieurs lieux dans la liste
   * @param {Array<string>} placeNames - Noms des lieux à surligner
   * @param {boolean} showIndicators - Afficher les indicateurs d'événements
   * @param {string} specificEventType - Type d'événement spécifique (optionnel)
   */
  highlightPlaces(placeNames, showIndicators = true, specificEventType = null) {
    if (!placeNames || placeNames.length === 0) return;
    
    // Vérifier que les références sont disponibles
    if (!lieux || sortMode === null) {
      console.error('❌ Références non initialisées dans ListHighlightHandler');
      return;
    }
    
    // Nettoyer l'état précédent
    this.clearHighlights();
    
    const elementsToShow = [];
    
    placeNames.forEach(placeName => {
      const placeData = lieux[placeName];
      if (!placeData?.domElement) return;
      
      const placeElement = placeData.domElement;
      
      // Marquer comme surligné
      placeElement.classList.add(this.config.classes.personMatch);
      HighlightStateManager.trackClassOperation(placeElement, [this.config.classes.personMatch]);
      
      this.state.highlightedPlaces.add(placeName);
      
      // Expansion du nom si nécessaire (sous-lieux en mode alphabétique)
      if (placeData.isSubLocation && sortMode === 'alphabetical') {
        this.expandPlaceName(placeElement, placeName);
      }
      
      // Ajout des indicateurs si demandé
      if (showIndicators && placeData.indicatorElement) {
        let events;
        
        if (specificEventType) {
          // Pour les mariages depuis un secteur, montrer seulement M
          events = [specificEventType];
        } else {
          // Sinon, montrer tous les événements du lieu
          events = this.getPlaceEvents(placeData);
        }
        
        if (events.length > 0) {
          this.addEventIndicators(placeData.indicatorElement, events);
        }
      }
      
      // Collecter pour l'overflow
      elementsToShow.push({
        element: placeElement,
        index: placeData.visualIndex,
        placeName: placeName
      });
    });
    
    // Griser les autres éléments
    this.grayOutOthers();
    
    // Gérer l'overflow après un court délai
    if (elementsToShow.length > 0) {
      setTimeout(() => {
        this.handleOverflow(elementsToShow);
      }, this.config.overflow.animationDelay);
    }
  },
  
  /**
   * Ajoute des indicateurs d'événements à un conteneur
   */
  addEventIndicators(container, events) {
    if (!container || !events || events.length === 0) return;
    
    const indicators = [];
    
    events.forEach((eventType, index) => {
      // Créer l'indicateur
      const indicator = document.createElement('div');
      indicator.className = `${this.config.indicators.indicatorClass} ${Events.cssClass(eventType)}`;
      indicator.textContent = Events.label(eventType);
      container.appendChild(indicator);
      indicators.push(indicator);
      
      // Ajouter un saut de ligne si nécessaire (4+ événements)
      if (index === this.config.indicators.maxBeforeBreak - 1 && events.length >= 4) {
        const breaker = document.createElement('div');
        breaker.className = this.config.indicators.lineBreakClass;
        container.appendChild(breaker);
        indicators.push(breaker);
        
        // Ajuster la hauteur de la ligne
        const row = container.closest('.place-row');
        if (row) {
          row.classList.add(this.config.classes.tallRow);
          HighlightStateManager.trackClassOperation(row, [this.config.classes.tallRow]);
        }
      }
    });
    
    // Enregistrer pour le nettoyage ultérieur
    this.state.addedIndicators.set(container, indicators);
  },
  
  /**
   * Grise les éléments non surlignés
   */
  grayOutOthers() {
    const allPlaceContents = document.querySelectorAll('.place-content');
    
    allPlaceContents.forEach(element => {
      const placeName = element.dataset.place;
      
      if (!this.state.highlightedPlaces.has(placeName)) {
        element.classList.add(this.config.classes.grayedOut);
        HighlightStateManager.trackClassOperation(element, [this.config.classes.grayedOut]);
      }
    });
  },
  
  /**
   * Expand le nom d'un sous-lieu pour afficher le nom complet
   */
  expandPlaceName(placeElement, placeName) {
    const nameElement = placeElement.querySelector('.place-name');
    if (!nameElement) return;
    
    // Sauvegarder le HTML original
    const originalHTML = nameElement.innerHTML;
    this.state.expandedNames.set(nameElement, originalHTML);
    HighlightStateManager.trackModification('dom', nameElement, { innerHTML: originalHTML });
    
    // Afficher le nom complet
    nameElement.textContent = placeName;
    nameElement.classList.add(this.config.classes.expandedName);
    HighlightStateManager.trackClassOperation(nameElement, [this.config.classes.expandedName]);
  },
  
  /**
   * Obtient les événements d'un lieu
   */
  getPlaceEvents(placeData) {
    return Events.types.filter(eventType => 
      placeData[Events.svgPrefix(eventType)]
    );
  },
  
  /**
   * Gère l'overflow pour les éléments hors de vue
   */
  handleOverflow(elementsToShow) {
    const list = document.querySelector('.places-list');
    if (!list) return;
    
    const scrollTop = list.scrollTop;
    const scrollBottom = scrollTop + list.clientHeight;
    
    const overflowAbove = [];
    const overflowBelow = [];
    
    // Déterminer quels éléments sont hors de vue
    elementsToShow.forEach(item => {
      const row = item.element.closest('.place-row');
      if (!row) return;
      
      const itemTop = row.offsetTop;
      const itemBottom = itemTop + row.offsetHeight;
      
      // L'élément est-il complètement visible ?
      const isCompletelyVisible = (itemTop >= scrollTop && itemBottom <= scrollBottom);
      
      if (!isCompletelyVisible) {
        const itemCenter = (itemTop + itemBottom) / 2;
        const viewCenter = (scrollTop + scrollBottom) / 2;
        
        if (itemCenter < viewCenter) {
          overflowAbove.push(item);
        } else {
          overflowBelow.push(item);
        }
      }
    });
    
    // Créer les sections d'overflow si nécessaire
    if (overflowAbove.length > 0 || overflowBelow.length > 0) {
      this.createOverflowSections(overflowAbove, overflowBelow);
    }
  },
  
  /**
   * Crée les sections d'overflow au-dessus et en-dessous
   */
  createOverflowSections(above, below) {
    const container = document.querySelector('.places-container');
    if (!container) return;
    
    // Section au-dessus
    if (above.length > 0) {
      const section = this.createOverflowSection('above', above);
      container.appendChild(section);
      this.state.overflowSections.push(section);
    }
    
    // Section en-dessous
    if (below.length > 0) {
      const section = this.createOverflowSection('below', below);
      container.appendChild(section);
      this.state.overflowSections.push(section);
    }
  },
  
  /**
   * Crée une section d'overflow individuelle
   */
  createOverflowSection(position, items) {
    const section = document.createElement('div');
    section.className = `${this.config.overflow.sectionClass} ${position}`;
    
    // Header
    const header = document.createElement('div');
    header.className = this.config.overflow.headerClass;
    header.innerHTML = `
      <i class="fas fa-arrow-${position === 'above' ? 'up' : 'down'} fa-sm"></i>
      ${items.length} lieu${items.length > 1 ? 'x' : ''} hors écran
    `;
    
    // Contenu
    const content = document.createElement('div');
    content.className = this.config.overflow.contentClass;
    
    // Ajouter les éléments (max 5)
    items.slice(0, this.config.overflow.maxItems).forEach(item => {
      const row = item.element.closest('.place-row');
      if (row) {
        const clone = row.cloneNode(true);
        
        // Nettoyer les IDs pour éviter les conflits
        clone.querySelectorAll('[id]').forEach(el => el.removeAttribute('id'));
        
        // S'assurer que l'élément n'est pas grisé
        const placeContent = clone.querySelector('.place-content');
        if (placeContent) {
          placeContent.classList.remove(this.config.classes.grayedOut);
        }
        
        content.appendChild(clone);
      }
    });
    
    section.appendChild(header);
    section.appendChild(content);
    
    return section;
  },
  
  /**
   * Nettoie tous les surlignages de la liste
   */
  clearHighlights() {
    // Nettoyer les indicateurs ajoutés
    this.state.addedIndicators.forEach((indicators, container) => {
      indicators.forEach(indicator => indicator.remove());
    });
    this.state.addedIndicators.clear();
    
    // Nettoyer les noms expandés
    this.state.expandedNames.clear();
    
    // Nettoyer les sections d'overflow
    this.state.overflowSections.forEach(section => section.remove());
    this.state.overflowSections = [];
    
    // Réinitialiser l'état
    this.state.highlightedPlaces.clear();
  },
  
  /**
   * Met en évidence les totaux d'événements dans le résumé
   */
  highlightEventTotal(eventType, highlight = true) {
    const labelSelector = `.summary-event-label.${Events.cssClass(eventType)}-label`;
    const label = document.querySelector(labelSelector);
    
    if (!label) return;
    
    if (highlight) {
      label.classList.add('active-hover');
      HighlightStateManager.trackClassOperation(label, ['active-hover']);
    }
  }
};

/**
 * HighlightCoordinator - Coordinateur principal du système de surlignage
 *
 * Ce module orchestre tous les handlers de surlignage et gère les interactions
 * bidirectionnelles entre le SVG et la liste des lieux. Il sert de point d'entrée
 * unique pour toutes les opérations de surlignage.
 */
const HighlightCoordinator = {
  // Configuration des comportements
  config: {
    enableBidirectional: true,  // Active le surlignage bidirectionnel SVG <-> Liste
    enableAgeHighlights: true,  // Active le surlignage des âges au décès
    enableImplexHighlights: true,  // Active le surlignage des implexes
    debugMode: false  // Mode debug pour tracer les opérations
  },

  /**
   * Initialise le système de surlignage
   */
  initialize() {
    // Mettre à jour l'état des modes visuels
    HighlightStateManager.updateVisualModes();

    // Configuration initiale si nécessaire
    this.config.enableBidirectional = HighlightStateManager.state.visualModes.placeColorActive;

    console.log('✨ Système de surlignage initialisé');
  },

  /**
   * Gère le survol d'un secteur SVG (personne ou mariage)
   * @param {SVGElement} sector - Le secteur survolé
   * @param {Object} person - Les données de la personne/mariage
   * @param {string} type - 'person' ou 'marriage'
   * @param {Event} event - L'événement de survol
   */
  handleSectorHover(sector, person, type, event) {
    if (this.config.debugMode) {
      console.log(`🎯 Survol secteur ${type}:`, person);
    }
    
    // Démarrer un nouveau surlignage
    if (!HighlightStateManager.startHighlight('sector', type, { person, sector }, sector)) {
      return;
    }
    
    // 1. Surligner le secteur SVG lui-même
    SVGHighlightHandler.highlightSector(sector, person, type);
    
    // 2. Si le mode colorisation est actif, surligner dans la liste
    if (this.config.enableBidirectional && HighlightStateManager.state.visualModes.placeColorActive) {
      const places = this.extractPlacesFromPerson(person, type);
      
      if (places.length > 0) {
        const placeNames = places.map(p => p.place);
        
        // IMPORTANT: Toujours montrer les indicateurs pour le survol secteur
        const showIndicators = true;
        
        // CORRECTION POUR MARIAGES: Si c'est un mariage, on ne montre que M
        const specificEventType = (type === 'marriage') ? 'marriage' : null;
        
        ListHighlightHandler.highlightPlaces(placeNames, showIndicators, specificEventType);
      }
    }
    
    // 3. Gérer les surlignages spéciaux
    this.handleSpecialHighlights(person, type, true);
  },

  /**
   * Gère la fin du survol d'un secteur SVG
   * @param {Object} person - Les données de la personne/mariage
   * @param {string} type - 'person' ou 'marriage'
   */
  handleSectorLeave(person, type) {
    if (this.config.debugMode) {
      console.log(`🎯 Fin survol secteur ${type}`);
    }

    // Nettoyer tous les surlignages
    HighlightStateManager.clearAllHighlights();

    // Gérer les surlignages spéciaux
    this.handleSpecialHighlights(person, type, false);
  },

  /**
   * Gère le survol d'un lieu dans la liste
   * @param {string} placeName - Nom du lieu survolé
   * @param {HTMLElement} placeElement - Élément DOM du lieu
   */
  handlePlaceHover(placeName, placeElement) {
    if (this.config.debugMode) {
      console.log(`📍 Survol lieu: ${placeName}`);
    }
    
    // Vérifier que lieux est disponible
    if (!lieux) {
      console.error('❌ Références non initialisées. Appelez initializeHighlightReferences d\'abord.');
      return;
    }
    
    const placeData = lieux[placeName];
    if (!placeData) {
      console.warn(`⚠️ Lieu non trouvé: ${placeName}`);
      return;
    }
    
    // Démarrer un nouveau surlignage
    if (!HighlightStateManager.startHighlight('place', 'place', { placeName, placeData }, placeElement)) {
      return;
    }
    
    // 1. Surligner dans la liste avec indicateurs (TOUJOURS selon la spec)
    ListHighlightHandler.highlightPlaces([placeName], true);
    
    // 2. Surligner dans le SVG avec colorisation appropriée
    const events = this.getPlaceEvents(placeData);
    SVGHighlightHandler.highlightByPlace(placeName, placeData, events);
  },

  /**
   * Gère la fin du survol d'un lieu
   */
  handlePlaceLeave() {
    if (this.config.debugMode) {
      console.log(`📍 Fin survol lieu`);
    }

    HighlightStateManager.clearAllHighlights();
  },

  /**
   * Gère le survol d'un total d'événement (NBMDS)
   * @param {string} eventType - Type d'événement ('birth', 'marriage', etc.)
   * @param {HTMLElement} labelElement - Élément du label survolé
   */
  handleEventTotalHover(eventType, labelElement) {
    if (this.config.debugMode) {
      console.log(`📊 Survol total événement: ${eventType}`);
    }
    
    if (!Events.isValid(eventType)) return;
    
    // Vérifier que lieux est disponible
    if (!lieux) {
      console.error('❌ Références non initialisées');
      return;
    }
    
    // Démarrer un nouveau surlignage
    if (!HighlightStateManager.startHighlight('event-total', eventType, { eventType }, labelElement)) {
      return;
    }
    
    // 1. Mettre en évidence le label
    ListHighlightHandler.highlightEventTotal(eventType, true);
    
    // 2. Collecter tous les lieux avec cet événement
    const placesToHighlight = [];
    
    Object.entries(lieux).forEach(([placeName, placeData]) => {
      if (placeData[Events.svgPrefix(eventType)]) {
        placesToHighlight.push(placeName);
      }
    });
    
    // 3. Surligner dans la liste SANS indicateurs (spec: "PAS d'indicateurs ajoutés")
    if (placesToHighlight.length > 0) {
      ListHighlightHandler.highlightPlaces(placesToHighlight, false, eventType);
    }
    
    // 4. Surligner dans le SVG selon le type d'événement
    // IMPORTANT: Pour les mariages, seuls les secteurs de mariage sont colorés
    SVGHighlightHandler.highlightByEventType(eventType);
  },

  // Méthode helper pour obtenir les événements d'un lieu
  getPlaceEvents(placeData) {
    return Events.types.filter(eventType => 
      placeData[Events.svgPrefix(eventType)]
    );
  },

  /**
   * Gère la fin du survol d'un total d'événement
   */
  handleEventTotalLeave() {
    if (this.config.debugMode) {
      console.log(`📊 Fin survol total événement`);
    }

    HighlightStateManager.clearAllHighlights();
  },

  /**
   * Gère le survol d'une légende d'âge au décès
   * @param {string} ageClass - Classe d'âge (DA0-DA6 ou DAM0-DAM6)
   * @param {HTMLElement} legendElement - Élément de la légende
   */
  handleAgeLegendHover(ageClass, legendElement) {
    if (!this.config.enableAgeHighlights) return;

    if (this.config.debugMode) {
      console.log(`💀 Survol légende âge: ${ageClass}`);
    }

    // Démarrer un nouveau surlignage
    if (!HighlightStateManager.startHighlight('age-legend', 'age', { ageClass }, legendElement)) {
      return;
    }

    // Surligner les secteurs SVG correspondants
    SVGHighlightHandler.highlightAgeGroup(ageClass, true);

    // Ajouter la classe de surlignage à la légende elle-même
    legendElement.classList.add('hl');
    HighlightStateManager.trackClassOperation(legendElement, ['hl']);
  },

  /**
   * Gère la fin du survol d'une légende d'âge
   */
  handleAgeLegendLeave() {
    if (this.config.debugMode) {
      console.log(`💀 Fin survol légende âge`);
    }

    HighlightStateManager.clearAllHighlights();
  },

  /**
   * Extrait les lieux associés à une personne
   * @private
   */
  extractPlacesFromPerson: function(person, type) {
    const places = [];
    
    if (type === 'marriage' && person.marriage_place) {
      places.push({
        place: person.marriage_place,
        event: 'marriage'
      });
    } else if (type === 'person') {
      // Utiliser directement les champs de person
      if (person.birth_place) places.push({ place: person.birth_place, event: 'birth' });
      if (person.baptism_place) places.push({ place: person.baptism_place, event: 'baptism' });
      if (person.death_place) places.push({ place: person.death_place, event: 'death' });
      if (person.burial_place) places.push({ place: person.burial_place, event: 'burial' });
    }
    
    console.log(`📍 Lieux extraits pour ${type}:`, places);
    return places;
  },

  /**
   * Gère les surlignages spéciaux (âges, implexes)
   * @private
   */
  handleSpecialHighlights(person, type, highlight) {
    // Surlignage des âges au décès
    if (this.config.enableAgeHighlights && type === 'person' && person.death_age) {
      const ageClass = Utils.deathAgeClass(person.death_age);
      const ageElement = document.getElementById(ageClass);

      if (ageElement) {
        if (highlight) {
          ageElement.classList.add('hl');
          HighlightStateManager.trackClassOperation(ageElement, ['hl']);
        } else {
          ageElement.classList.remove('hl');
        }
      }
    }

    // Surlignage des durées de mariage
    if (this.config.enableAgeHighlights && type === 'marriage' && person.marriage_length) {
      const marriageClass = Utils.marriageLengthClass(person.marriage_length);
      if (marriageClass) {
        const marriageElement = document.getElementById(marriageClass);

        if (marriageElement) {
          if (highlight) {
            marriageElement.classList.add('hl');
            HighlightStateManager.trackClassOperation(marriageElement, ['hl']);
          } else {
            marriageElement.classList.remove('hl');
          }
        }
      }
    }

    // Surlignage des implexes
    if (this.config.enableImplexHighlights && person.sosasame) {
      const refElement = document.getElementById("S" + person.sosasame);

      if (refElement) {
        if (highlight) {
          refElement.classList.add('same_hl');
          HighlightStateManager.trackClassOperation(refElement, ['same_hl']);
        } else {
          refElement.classList.remove('same_hl');
        }
      }
    }
  },

  /**
   * Active/désactive le mode debug
   */
  setDebugMode(enabled) {
    this.config.debugMode = enabled;

    if (enabled) {
      console.log('🐛 Mode debug activé pour le système de surlignage');
      HighlightStateManager.debugState();
    }
  },

  /**
   * Méthode utilitaire pour tester le système
   */
  testHighlightSystem() {
    console.group('🧪 Test du système de surlignage');

    // Test 1: Surlignage d'un lieu
    console.log('Test 1: Surlignage du premier lieu...');
    const firstPlace = Object.keys(lieux)[0];
    if (firstPlace) {
      this.handlePlaceHover(firstPlace, lieux[firstPlace].domElement);
      setTimeout(() => {
        this.handlePlaceLeave();
        console.log('✅ Test 1 terminé');
      }, 2000);
    }

    console.groupEnd();
  }
};

/**
 * IntegrationHelper - Guide d'intégration et migration vers la nouvelle architecture
 *
 
 */
const IntegrationHelper = {
  /**
   * Initialise le système de surlignage
   */
  initializeHighlightSystem: function() {
    // 1. S'assurer que les données sont prêtes
    if (!lieux || Object.keys(lieux).length === 0) {
      console.error('❌ Système de surlignage : lieux non disponibles');
      return;
    }
    
    // 2. Générer les styles CSS
    HighlightCSSGenerator.generateLocationStyles();
    
    // 3. Initialiser le coordinateur
    HighlightCoordinator.initialize();
    
    // 4. Configurer les événements après un court délai
    setTimeout(() => {
      IntegrationHelper.setupSVGEventListeners();
      
      if (document.querySelector('.places-panel')) {
        IntegrationHelper.setupListEventListeners();
      }
      
      if (document.body.classList.contains('death-age')) {
        IntegrationHelper.setupLegendEventListeners();
      }
      
      console.log('✅ Système de surlignage entièrement initialisé');
      
      // 5. Test automatique en mode debug
      if (window.location.search.includes('debug=highlight')) {
        setTimeout(() => {
          console.log('🧪 Test automatique du système de surlignage...');
          HighlightCoordinator.testHighlightSystem();
        }, 1000);
      }
    }, 200); // Délai pour s'assurer que le DOM est stable
  },

  initializeHighlightReferences: function() {
      // Vérifier et logger l'état des références
      console.log('🔧 Initialisation des références de surlignage');
      console.log('- lieux disponibles:', Object.keys(lieux).length);
      console.log('- sortMode:', sortMode);
      console.log('- showEvents:', showEvents);
      
      // Injecter les références si nécessaire
      if (typeof ListHighlightHandler !== 'undefined') {
          ListHighlightHandler._lieux = lieux;
          ListHighlightHandler._sortMode = sortMode;
      }
  },

  verifyHighlightClasses: function() {
      console.group('🔍 Vérification des classes CSS de surlignage');
      
      // Vérifier les styles de lieu
      const sheet = [...document.styleSheets].find(s => s.title === "fc-auto");
      if (sheet) {
          const rules = Array.from(sheet.cssRules);
          const locationRules = rules.filter(r => r.selectorText?.includes('-L'));
          console.log(`Classes de lieu trouvées: ${locationRules.length}`);
      }
      
      // Vérifier les éléments SVG
      Events.types.forEach(eventType => {
          const prefix = Events.svgPrefix(eventType);
          const elements = document.querySelectorAll(`[class*="${prefix}-L"]`);
          console.log(`Éléments ${eventType}: ${elements.length}`);
      });
      
      console.groupEnd();
  },

  /**
   * Configure les événements de surlignage sur les éléments SVG
   * Remplace complètement les anciens handleMouseEnter/handleMouseLeave de SVGRenderer
   */
  setupSVGEventListeners: function() {
    console.log('🔗 Configuration des événements SVG pour le surlignage');
    
    // Utiliser une délégation d'événements sur le SVG entier
    const svg = document.getElementById('fanchart');
    if (!svg) return;
    
    // Mouseenter avec capture pour intercepter avant les handlers existants
    svg.addEventListener('mouseenter', (e) => {
      const link = e.target.closest('.link');
      if (!link) return;
      
      const group = link.closest('g[id^="S"], g[id^="M"]');
      if (!group) return;
      
      const id = group.id;
      const sosa = parseInt(id.substring(1));
      
      // Déterminer le type et récupérer les données
      let person, type;
      if (id.startsWith('S')) {
        person = ancestor["S" + sosa];
        type = 'person';
      } else if (id.startsWith('M')) {
        // Pour les mariages, récupérer depuis le père
        const fatherSosa = sosa;
        person = ancestor["S" + fatherSosa];
        type = 'marriage';
      }
      
      if (person) {
        // Appeler le coordinateur APRÈS le handler existant
        setTimeout(() => {
          HighlightCoordinator.handleSectorHover(link, person, type, e);
        }, 0);
      }
    }, true); // Capture phase
    
    // Mouseleave
    svg.addEventListener('mouseleave', (e) => {
      const link = e.target.closest('.link');
      if (!link) return;
      
      const group = link.closest('g[id^="S"], g[id^="M"]');
      if (!group) return;
      
      const id = group.id;
      const sosa = parseInt(id.substring(1));
      
      let person, type;
      if (id.startsWith('S')) {
        person = ancestor["S" + sosa];
        type = 'person';
      } else if (id.startsWith('M')) {
        const fatherSosa = sosa;
        person = ancestor["S" + fatherSosa];
        type = 'marriage';
      }
      
      if (person) {
        setTimeout(() => {
          HighlightCoordinator.handleSectorLeave(person, type);
        }, 0);
      }
    }, true);
  },

  /**
   * Configure les événements sur la liste des lieux
   * Remplace le code existant dans PlacesInterface.setupEventListeners
   */
  setupListEventListeners() {
    const placesList = document.querySelector('.places-list');
    if (!placesList) return;

    // Survol des lieux
    placesList.addEventListener('mouseenter', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        const placeName = placeContent.dataset.place;
        if (placeName) {
          HighlightCoordinator.handlePlaceHover(placeName, placeContent);
        }
      }
    }, true);

    placesList.addEventListener('mouseleave', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        HighlightCoordinator.handlePlaceLeave();
      }
    }, true);

    // Survol des totaux d'événements
    document.querySelectorAll('.summary-event-label').forEach(label => {
      // Déterminer le type d'événement à partir du texte du label
      const eventType = this.getEventTypeFromLabel(label);
      if (!eventType) return;

      label.addEventListener('mouseenter', () => {
        HighlightCoordinator.handleEventTotalHover(eventType, label);
      });

      label.addEventListener('mouseleave', () => {
        HighlightCoordinator.handleEventTotalLeave();
      });
    });
  },

  /**
   * Configure les événements sur les légendes d'âge
   * Remplace le code existant dans LegendManager
   */
  setupLegendEventListeners() {
    // Légendes d'âge au décès
    const ageClasses = ["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6"];
    const marriageClasses = ["DAM0", "DAM1", "DAM2", "DAM3", "DAM4", "DAM5", "DAM6"];

    [...ageClasses, ...marriageClasses].forEach(ageClass => {
      const element = document.getElementById(ageClass);
      if (!element) return;

      element.addEventListener('mouseenter', () => {
        HighlightCoordinator.handleAgeLegendHover(ageClass, element);
      });

      element.addEventListener('mouseleave', () => {
        HighlightCoordinator.handleAgeLegendLeave();
      });
    });
  },

  /**
   * Détermine le type d'événement à partir d'un label
   * @private
   */
  getEventTypeFromLabel: function(label) {
    // Utiliser les classes CSS au lieu du texte
    if (label.classList.contains('birth-label')) return 'birth';
    if (label.classList.contains('baptism-label')) return 'baptism';
    if (label.classList.contains('marriage-label')) return 'marriage';
    if (label.classList.contains('death-label')) return 'death';
    if (label.classList.contains('burial-label')) return 'burial';
    
    // Fallback sur le texte
    const labelText = label.textContent.trim();
    const labelToEvent = {
      'N': 'birth',
      'B': 'baptism', 
      'M': 'marriage',
      'D': 'death',
      'S': 'burial'
    };
    
    return labelToEvent[labelText] || null;
  },

  /**
   * Teste la migration en vérifiant que tous les composants sont présents
   */
  verifyMigration() {
    console.group('🔍 Vérification de la migration');

    const checks = [
      {
        name: 'HighlightStateManager',
        test: () => typeof HighlightStateManager !== 'undefined'
      },
      {
        name: 'SVGHighlightHandler',
        test: () => typeof SVGHighlightHandler !== 'undefined'
      },
      {
        name: 'ListHighlightHandler',
        test: () => typeof ListHighlightHandler !== 'undefined'
      },
      {
        name: 'HighlightCoordinator',
        test: () => typeof HighlightCoordinator !== 'undefined'
      },
      {
        name: 'Ancien PlacesHighlighter supprimé',
        test: () => typeof PlacesHighlighter === 'undefined'
      }
    ];

    let allPassed = true;

    checks.forEach(check => {
      const passed = check.test();
      console.log(`${passed ? '✅' : '❌'} ${check.name}`);
      if (!passed) allPassed = false;
    });

    console.log(allPassed ? '✅ Migration réussie!' : '❌ Migration incomplète');
    console.groupEnd();

    return allPassed;
  }
};

const DOMCache = {
  // Cache pour les éléments individuels
  elements: {},

  // Cache pour les collections d'éléments par classe
  collections: {},

  // Récupérer un élément par ID avec mise en cache
  getElementById: function(id) {
    if (!this.elements[id]) {
      this.elements[id] = document.getElementById(id);
    }
    return this.elements[id];
  },

  // Récupérer des éléments par classe avec mise en cache
  getElementsByClassName: function(className) {
    if (!this.collections[className]) {
      // Convertir en Array pour avoir une référence stable
      this.collections[className] = Array.from(document.getElementsByClassName(className));
    }
    return this.collections[className];
  },

  // Invalider le cache quand le DOM change
  invalidate: function(type = 'all') {
    if (type === 'all') {
      this.elements = {};
      this.collections = {};
    } else if (type === 'collections') {
      this.collections = {};
    }
  },

  // Pré-charger les éléments fréquemment utilisés
  preload: function() {
    // Boutons fréquemment utilisés
    ["b-death-age", "b-places-colorise", "b-sort-places"].forEach(id => {
      this.getElementById(id);
    });
    // s indicateurs d'âge et de mariage
    ["DA0", "DA1", "DA2", "DA3", "DA4", "DA5", "DA6"].forEach(id => {
      this.getElementById(id);
    });
    ["DAM0", "DAM1", "DAM2", "DAM3", "DAM4", "DAM5", "DAM6"].forEach(id => {
      this.getElementById(id);
    });
  }
};

const LayoutCalculator = {
  /**
   * Mesure la largeur nécessaire pour afficher confortablement la liste des lieux
   * Utilise un élément temporaire pour mesurer le texte réel
   */
  calculatePlacesListWidth: function() {
    // Si pas de liste de lieux, retourner une largeur minimale
    if (!document.body.classList.contains('place_color')) {
      return 0;
    }

    // Créer un élément de mesure temporaire
    const measurer = document.createElement('div');
    measurer.style.cssText = `
      position: absolute;
      visibility: hidden;
      white-space: nowrap;
      font-family: inherit;
      font-size: inherit;
    `;
    document.body.appendChild(measurer);

    let maxWidth = 200; // Largeur minimale par défaut

    // Mesurer chaque lieu
    lieux_a.forEach(([placeName, data]) => {
      // Construire le texte complet comme il apparaîtra
      let text = '';

      // Indicateurs d'événements (N B M D S)
      if (has_bi && data.bi) text += 'N ';
      if (has_ba && data.ba) text += 'B ';
      if (has_ma && data.ma) text += 'M ';
      if (has_de && data.de) text += 'D ';
      if (has_bu && data.bu) text += 'S ';

      // Carré coloré et nom du lieu
      text += '■ ' + placeName;

      measurer.textContent = text;
      const width = measurer.offsetWidth;

      if (width > maxWidth) {
        maxWidth = width;
      }
    });

    // Nettoyer
    document.body.removeChild(measurer);

    // Ajouter des marges (padding, scrollbar, etc.)
    return maxWidth + 40; // 20px de chaque côté pour le confort
  },

  /**
   * Calcule la largeur maximale disponible pour le fanchart
   * en tenant compte du contenu réel
   */
  calculateMaxFanchartWidth: function() {
    const windowWidth = window.innerWidth;
    const placesListWidth = this.calculatePlacesListWidth();

    // Pour le mode 359° (très carré), ajouter une marge à gauche
    const isSquareChart = (current_angle >= 310 || isCircularMode);
    const leftMargin = isSquareChart ? 150 : 0; // Espace pour les contrôles

    // Calculer l'espace disponible
    const availableWidth = windowWidth - placesListWidth - leftMargin;

    // S'assurer qu'on utilise au moins 50% de l'écran pour le graphique
    return Math.max(availableWidth, windowWidth * 0.5);
  }
};


// ========== URLManager centralisé ==========
const URLManager = {
  config: {
    basePerson: link_to_person, // URL de base
    defaultParams: { module: 'A', template: 'FC' }, // Paramètres par défaut
    specialParams: { place: { module: 'MOD_DATA', data: 'place' } } // Paramètres spéciaux
  },

  readCurrentState: function() {
    const urlParams = new URLSearchParams(window.location.search);

    return {
      tool: urlParams.get('tool') || '',
      sortMode: urlParams.has('sort') ? 'alphabetical' : 'frequency',
      showEvents: urlParams.has('events'),
      isCircular: urlParams.get('mode') === 'couple',
      angle: parseInt(urlParams.get('angle')) || 220,
      implexMode: urlParams.get('implex') === 'num' ? 'numbered' :
                  urlParams.get('implex') === 'full' ? 'full' : 'reduced'
    };
  },

  /**
   * Méthode principale : construit une URL pour une personne
   *
   * @param {Object} person - Objet personne avec fnk, snk, oc
   * @param {Object} options - Options de construction de l'URL
   * @returns {string} URL complète
   */
  buildPersonURL: function(person, options = {}) {
    // Options par défaut avec fusion intelligente
    const opts = {
      // Comportement de navigation
      useCurrentState: true,     // Inclure l'état actuel (générations, mode, etc.)
      externalNavigation: false, // true = fiche individuelle, false = fanchart
      targetGeneration: null,    // Forcer une génération spécifique

      // Préservation de l'état existant
      preserveTools: true,       // Garder les outils actifs (colorisation, etc.)
      preserveMode: true,        // Garder le mode (circulaire, angle)
      preserveView: true,        // Garder la vue (zoom, position)

      // Fusion avec les options passées
      ...options
    };

    // Construction de l'URL selon le type de navigation
    if (opts.externalNavigation) {
      // Navigation externe : URL simple vers la fiche individuelle
      return this._buildExternalPersonURL(person);
    } else {
      // Navigation interne : URL complète avec état du fanchart
      return this._buildFanchartPersonURL(person, opts);
    }
  },

  /**
   * Construit une URL pour une recherche de lieu
   *
   * @param {string} placeName - Nom du lieu à rechercher
   * @param {Object} options - Options de recherche
   * @returns {string} URL de recherche
   */
  buildPlaceURL: function(placeName, options = {}) {
    const opts = {
      exactSearch: true,    // Recherche exacte ou partielle
      searchPrefix: true,   // Inclure une recherche par préfixe
      ...options
    };

    const baseURL = this.config.basePerson;
    const params = [];

    // Paramètres de base pour la recherche de lieux
    params.push(`m=${this.config.specialParams.place.module}`);
    params.push(`data=${this.config.specialParams.place.data}`);

    // Logique de recherche intelligente
    if (opts.exactSearch && placeName.length > 2) {
      // Recherche principale avec nom tronqué (logique existante)
      const searchTerm = placeName.slice(0, -2);
      params.push(`s=${encodeURIComponent(searchTerm)}`);
    }

    // Recherche exacte complémentaire
    params.push(`s1=${encodeURIComponent(placeName)}`);

    return baseURL + params.join('&');
  },

  /**
   * Met à jour l'URL de la page courante avec l'état actuel
   *
   * @param {Object} stateOverrides - Remplacements ponctuels de l'état
   */
  updateCurrentURL: function(stateOverrides = {}) {
    const person = ancestor["S1"];
    if (!person) return;

    // Construire l'URL avec l'état actuel + les remplacements
    const options = {
      useCurrentState: true,
      preserveTools: true,
      preserveMode: true,
      preserveView: false, // Ne pas préserver le zoom pour les URL
      ...stateOverrides
    };

    const newURL = this.buildPersonURL(person, options);

    // Mise à jour de l'historique sans rechargement
    history.replaceState(null, '', newURL);
  },

  /**
   * Navigation vers une nouvelle personne avec paramètres
   * Remplace Utils.navigateWithParams
   *
   * @param {number} targetGeneration - Génération cible
   * @param {Object} additionalOptions - Options supplémentaires
   */
  navigateToGeneration: function(targetGeneration, additionalOptions = {}) {
    const person = ancestor["S1"];
    if (!person) return;

    // Sauvegarder l'état actuel pour restauration
    const savedMaxGen = max_gen;

    // Construire l'URL avec la nouvelle génération
    max_gen = targetGeneration;
    const options = {
      targetGeneration: targetGeneration,
      useCurrentState: true,
      preserveTools: true,
      preserveMode: true,
      ...additionalOptions
    };

    const url = this.buildPersonURL(person, options);

    // Restaurer l'état et naviguer
    max_gen = savedMaxGen;
    window.location = url;
  },

  /**
   * Navigation directe vers une personne
   * Centralise les appels depuis les événements SVG et les clics
   *
   * @param {Object} person - Personne cible
   * @param {boolean} newTab - Ouvrir dans un nouvel onglet
   * @param {boolean} stayInFanchart - Rester dans le fanchart vs fiche individuelle
   */
  navigateToPerson: function(person, newTab = false, stayInFanchart = false) {
    // Nettoyer le cache car on change de contexte
    if (!stayInFanchart && !newTab) {
      LocationDataBuilder.clearCache();
    }

    if (!person || !person.fnk || !person.snk) {
      console.warn('URLManager: Personne invalide pour navigation', person);
      return false;
    }

    const url = this.buildPersonURL(person, {
      externalNavigation: !stayInFanchart,
      useCurrentState: stayInFanchart
    });

    // Exécution de la navigation
    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }

    return true;
  },

  /**
   * Navigation vers un lieu (recherche)
   * @param {string} placeName - Nom du lieu
   * @param {boolean} newTab - Nouvel onglet
   */
  navigateToPlace: function(placeName, newTab = false) {
    const url = this.buildPlaceURL(placeName);

    if (newTab) {
      window.open(url, '_blank');
    } else {
      window.location.href = url;
    }

    return true;
  },

  // ========== MÉTHODES PRIVÉES ==========
  /**
   * Construit une URL externe simple (fiche individuelle)
   * @private
   */
  _buildExternalPersonURL: function(person) {
    const params = [`p=${person.fnk}`, `n=${person.snk}`];

    if (person.oc) {
      params.push(`oc=${person.oc}`);
    }

    return this.config.basePerson + params.join('&');
  },

  /**
   * Construit une URL complète pour navigation fanchart
   * @private
   */
  _buildFanchartPersonURL: function(person, options) {
    // Paramètres de base obligatoires
    const params = [
      `m=${this.config.defaultParams.module}`,
      `t=${this.config.defaultParams.template}`,
      `p=${person.fnk}`,
      `n=${person.snk}`
    ];

    // Paramètres optionnels de la personne
    if (person.oc) params.push(`oc=${person.oc}`);

    // État du fanchart si demandé
    if (options.useCurrentState) this._addFanchartState(params, options);

    return this.config.basePerson + params.join('&');
  },

  /**
   * Ajoute l'état actuel du fanchart aux paramètres
   * @private
   */
  _addFanchartState: function(params, options) {
    const targetGen = options.targetGeneration || max_gen;
    params.push(`v=${targetGen}`);

    if (options.preserveMode) {
      if (isCircularMode) {
        params.push('mode=couple');
      } else if (current_angle !== 220) {
        params.push(`angle=${current_angle}`);
      }
    }

    if (options.preserveTools) {
      if (tool) params.push(`tool=${tool}`);

      if (sortMode === 'alphabetical') params.push('sort');

      const placesPanel = document.querySelector('.places-panel');
      if (placesPanel?.classList.contains('show-events')) {
        params.push('events');
      }

      if (implexMode === 'numbered') params.push('implex=num');
      else if (implexMode === 'full') params.push('implex=full');

      if (has_ba) params.push('ba=on');
      if (has_bu) params.push('bu=on');
    }
  }
};

// ========== Module de construction des données de lieux ==========
const LocationDataBuilder = {
  _locationCache: new Map(),
  _generationCache: new Map(),

  /*
   * Fonction principale qui orchestre toute la construction des données de lieux
   * @param {number} maxGeneration - Génération maximum à considérer
   */
  buildCompleteLocationData: function(maxGeneration = null) {
    const targetGeneration = maxGeneration || max_gen;

    // Invalider le cache de tri car les lieux ont changé
    if (PlacesInterface.cache) {
        PlacesInterface.cache.invalidateSort();
    }

    // Vérifier le cache de génération
    if (this._generationCache.has(targetGeneration)) {
      console.log(`📦 Données de lieux récupérées du cache (gen ${targetGeneration})`);
      const cached = this._generationCache.get(targetGeneration);
      lieux = cached.lieux;
      lieux_a = cached.lieux_a;
      this.restoreGlobalFlags();
      return;
    }

    // Construction normale si pas en cache
    console.log(`🏗️ Construction des données de lieux (${targetGeneration} générations)`);

    this.resetLocationData();
    const filteredAncestors = this.filterAncestorsByGeneration(targetGeneration);

    // Traitement principal
    Object.values(filteredAncestors).forEach(person => {
      this.processPersonAllLocations(person);
    });

    // Construction de l'array final
    this.buildFinalLocationArray();

    // Mettre en cache pour cette génération
    this._generationCache.set(targetGeneration, {
      lieux: { ...lieux },
      lieux_a: [...lieux_a],
      flags: this.captureGlobalFlags()
    });

    console.log(`✅ ${Object.keys(lieux).length} lieux traités`);
  },

  /**
   * Filtre les ancêtres selon la génération maximum
   * Optimisé avec calcul direct des plages de Sosa
   */
  filterAncestorsByGeneration: function(maxGeneration) {
    const filteredAncestors = {};

    for (let gen = 1; gen <= maxGeneration + 1; gen++) {
      const startSosa = 1 << (gen - 1);  // Décalage à gauche de bits = 2^(gen-1)
      const endSosa = (1 << gen) - 1;    // Décalage à gauche de bits = 2^gen-1

      for (let sosa = startSosa; sosa <= endSosa; sosa++) {
        const key = "S" + sosa;
        if (ancestor[key]) {
          filteredAncestors[key] = ancestor[key];
        }
      }
    }

    return filteredAncestors;
  },

  /**
   * Réinitialise toutes les structures de données globales
   */
  resetLocationData: function() {
    lieux = {};
    lieux_a = [];

    // Reset des flags globaux via Events
    Events.types.forEach(eventType => {
      window[Events.flagProp(eventType)] = false;
    });
  },

  /**
   * Traite tous les lieux associés à une personne
   */
  processPersonAllLocations: function(person) {
    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType);

      if (person[placeField]?.trim()) {
        this.processSingleLocation(person[placeField], eventType, person);
      }
    });
  },

  /**
   * Traite un lieu spécifique pour un type d'événement donné
   * @param {string} placeName - Nom du lieu brut
   * @param {string} eventType - Type d'événement (bi, ba, ma, de, bu)
   * @param {Object} person - Référence à la personne (pour de futures extensions)
   * @returns {Object} Information sur la méthode utilisée
   */
  processSingleLocation: function(placeName, eventType, person) {
    // Nettoyage du nom de lieu
    const cleanPlaceName = placeName.replace(/^\?, /, "");

     // Initialisation de l'entrée si première occurrence de ce lieu
    if (!lieux[cleanPlaceName]) {
      const locationStructure = this.extractLocationStructure(cleanPlaceName, eventType, person);
      lieux[cleanPlaceName] = this.createLocationEntry(cleanPlaceName, locationStructure);
    }

    // Mise à jour des compteurs
    this.updateLocationCounters(lieux[cleanPlaceName], eventType), person.sosa;
  },

  /**
   * Extrait la structure du lieu
   */
  extractLocationStructure: function(placeName, eventType, person) {
    const subField = eventType + '_sub';
    const mainField = eventType + '_main';

    return {
      fullName: placeName,
      isSubLocation: !!person[subField],
      subName: person[subField] || null,
      parentLocation: person[mainField] || null
    };
  },

  /**
   * Crée une entrée de lieu optimisée avec les métadonnées nécessaires
   * @param {string} cleanPlaceName - Nom nettoyé du lieu
   * @param {Object} locationStructure - Structure du lieu
   * @returns {Object} Entrée complète pour l'objet lieux
   */
  createLocationEntry: function(cleanPlaceName, locationStructure) {
    const entry = {
      cnt: 0,
      c: null,
      maxGeneration: 0,

      // Métadonnées géographiques
      isSubLocation: locationStructure.isSubLocation,
      subName: locationStructure.subName,
      parentLocation: locationStructure.parentLocation,

      // Données DOM préparées
      domAttributes: {
        'data-place': cleanPlaceName,
        'data-is-sublocation': locationStructure.isSubLocation,
        'data-events': []
      }
    };

    // Initialisation des compteurs et flags via Events
    Events.types.forEach(eventType => {
      entry[Events.count(eventType)] = 0;
      entry[Events.svgPrefix(eventType)] = false;
    });

    return entry;
  },

  /**
   * Met à jour les compteurs pour un lieu donné avec tracking de la génération
   * @param {Object} locationEntry - Entrée de lieu dans l'objet lieux
   * @param {string} eventType - Type d'événement à incrémenter
   */
  updateLocationCounters: function(locationEntry, eventType, sosa) {
    const countField = Events.count(eventType);
    const flagField = Events.svgPrefix(eventType);

    // Calculer la génération depuis le Sosa
    const generation = sosa ? Math.floor(Math.log2(sosa)) + 1 : 0;
    locationEntry.maxGeneration = Math.max(locationEntry.maxGeneration, generation);

    // Incréments
    locationEntry[countField]++;
    locationEntry.cnt++;

    // Flags et DOM attributes
    if (!locationEntry[flagField]) {
      locationEntry[flagField] = true;
      window[Events.flagProp(eventType)] = true;
      locationEntry.domAttributes['data-events'].push(eventType);
    }
  },

  /**
   * Construit l’array final lieux_a et assigne les IDs CSS pour la colorisation
   * Transforme l’objet lieux en array utilisable par les fonctions de tri
   */
  buildFinalLocationArray: function() {
    // Conversion en array
    lieux_a = Object.entries(lieux);

    // Assignation des IDs et pré-calculs
    lieux_a.forEach(([placeName, locationData], index) => {
      locationData.c = "L" + index;
    });
  },

  /**
   * Capture l'état des flags globaux pour le cache
   */
  captureGlobalFlags: function() {
    const flags = {};
    Events.types.forEach(eventType => {
      flags[Events.flagProp(eventType)] = window[Events.flagProp(eventType)];
    });
    return flags;
  },

  /**
   * Restaure les flags depuis le cache
   */
  restoreGlobalFlags: function() {
    const cached = this._generationCache.get(max_gen);
    if (cached?.flags) {
      Object.entries(cached.flags).forEach(([flag, value]) => {
        window[flag] = value;
      });
    }
  },

  /**
   * Valide et corrige les relations parent-enfant
   * @returns {Object} Rapport de validation
   */
  validateLocationHierarchy: function() {
    let corrections = 0;
    let subLocations = 0;
    let orphans = [];

    Object.entries(lieux).forEach(([placeName, locationData]) => {
      if (locationData.isSubLocation && locationData.parentLocation) {
        subLocations++;

        // Vérifier l'existence du parent
        if (!lieux[locationData.parentLocation]) {
          orphans.push(placeName);

          // Correction automatique
          locationData.isSubLocation = false;
          locationData.subName = null;
          locationData.parentLocation = null;
          locationData.domAttributes['data-is-sublocation'] = false;
          corrections++;
        }
      }
    });

    if (orphans.length > 0) {
      console.warn(`⚠️ Sous-lieux orphelins corrigés:`, orphans);
    }

    return {
      corrections,
      subLocations,
      orphans,
      totalLocations: Object.keys(lieux).length
    };
  },

  /**
   * Pré-calcule les classes CSS pour optimisation
   */
  precomputeCSSClasses: function(locationData, index) {
    const classes = {
      colorClass: `color-${(index % 12) + 1}`,
      eventClasses: []
    };

    Events.types.forEach(eventType => {
      if (locationData[Events.svgPrefix(eventType)]) {
        classes.eventClasses.push(Events.cssClass(eventType));
      }
    });

    return classes;
  },

  /**
   * Statistiques par type d’événement
   * @returns {Object} Statistiques détaillées par type
   */
  getEventStatistics: function() {
    const stats = {};
    Events.types.forEach(eventType => { stats[eventType] = 0; });

    Object.values(lieux).forEach(locationData => {
      Events.types.forEach(eventType => {
        const count = Events.count(eventType);
        stats[eventType] += locationData[count] || 0;
      });
    });

    return stats;
  },

 /**
   * Nettoie le cache (appelé lors des changements majeurs)
   */
  clearCache: function() {
    this._locationCache.clear();
    this._generationCache.clear();
    console.log('🧹 Cache des lieux nettoyé');
  }
};

const HighlightCSSGenerator = {
  generateLocationStyles: function() {
    console.log('🎨 Génération des styles CSS de colorisation');
    
    // Nettoyer les anciennes règles
    for (let i = sheet.cssRules.length - 1; i >= 0; i--) {
      if (sheet.cssRules[i].selectorText?.includes('-L')) {
        sheet.deleteRule(i);
      }
    }
    
    // Générer les nouvelles règles
    lieux_a.forEach(([placeName, locationData], index) => {
      const hue = (index * 360 / lieux_a.length) % 360;
      const color = `hsl(${hue}, 60%, 65%)`;
      
      // Créer les règles pour chaque type d'événement
      Events.types.forEach(eventType => {
        const prefix = Events.svgPrefix(eventType);
        
        // Règle pour les secteurs
        sheet.insertRule(
          `body.place_color svg .${prefix}-L${index} { 
            fill: ${color} !important; 
            fill-opacity: 0.6 !important; 
          }`,
          sheet.cssRules.length
        );
        
        // Règle pour les textes
        sheet.insertRule(
          `body.place_color svg .${prefix}-tL${index} { 
            fill: black !important; 
          }`,
          sheet.cssRules.length
        );
      });
    });
    
    console.log(`✅ ${lieux_a.length * Events.types.length * 2} règles CSS générées`);
  }
};

// ========== Interface du panneau des lieux ==========
const PlacesInterface = {
  cache: { // Cache avec invalidation sélective
    elements: {},
    sortedPlaces: null,
    lastSortMode: null,
    fragment: null, // Réutilisation du fragment DOM

    invalidateSort: function() {
      this.sortedPlaces = null;
    }
  },

 initialize: function() {
    this.cache.elements = { // Cache des éléments critiques
      panel: document.querySelector('.places-panel'),
      placesList: document.querySelector('.places-list'),
      summaryPlaces: document.querySelector('.summary-places-info'),
      summaryEventCounts: document.querySelectorAll('.summary-event-count'),
      summaryTotal: document.querySelector('.summary-total-events'),
      summaryPersons: document.querySelector('.summary-persons-count')
    };

    if (!this.cache.elements.placesList) {
      console.error('❌ Éléments HTML requis manquants');
      return false;
    }

    this.generatePlacesList();
    this.updateSummarySection();
    this.setupNavigationListeners();

    return true;
  },

  placeRowTemplate: function(placeName, placeData, index) {
    // Calculer l'index de couleur (rotation sur 12 couleurs)
    const colorIndex = (index % 12) + 1;

    // Préparer les attributs data-* en une seule chaîne
    const dataAttributes = `
      data-place="${placeName}"
      data-place-class="${placeData.c || ''}"
      data-total="${placeData.cnt || 0}"
    `.trim();

    // Template HTML complet
    return `
      <div class="place-row" data-index="${index}">
        <div class="place-indicators" id="indic-${index}"></div>
        <div class="place-content" id="place-${index}" ${dataAttributes}>
          <div class="place-left">
            <div class="place-color color-${colorIndex}"></div>
            <div class="place-name">${this.formatPlaceName(placeName, placeData)}</div>
          </div>
          <div class="place-right">
            <div class="place-events">
              ${this.generateEventItemsHTML(placeData)}
            </div>
            <div class="place-count">${placeData.cnt || 0}</div>
          </div>
        </div>
      </div>
    `;
  },

  formatPlaceName: function(placeName, placeData) {
    if (sortMode === 'alphabetical' && placeData.isSubLocation) {
      // Mode alphabétique : afficher le sous-nom avec indentation
      const label = placeData.subName || placeName;
      return `<span class="sublocation-indicator">└ </span>${label}`;
    }
    // Mode fréquence ou lieu principal : afficher le nom complet
    return placeName;
  },

  generateEventItemsHTML: function(placeData) {
    const htmlParts = Events.types.map(eventType => {
      const label = Events.label(eventType);
      const eventCount = Events.count(eventType);
      const count = placeData[eventCount] || 0;
      const isActive = count > 0;

      return `
        <div class="event-item ${isActive ? 'active' : ''}" data-event="${eventType}">
          <span class="event-count">${count > 1 ? count : ''}</span>
          <span class="event-label">${label}</span>
        </div>
      `;
    });

    return htmlParts.join('');
  },

  /**
   * Génèration de la liste des lieux enrichie
   */
  generatePlacesList: function() {
    const container = this.cache.elements.placesList;
    if (!container) return;

    // Utiliser le cache de tri
    const sortedPlaces = this.getCachedSortedPlaces();

    // Fragment DOM pour performance
    const fragment = document.createDocumentFragment();

    sortedPlaces.forEach(([placeName, placeData], index) => {
      const row = this.createPlaceRow(placeName, placeData, index);

      // Enrichissement immédiat des références DOM
      placeData.domElement = row.querySelector('.place-content');
      placeData.visualIndex = index;
      placeData.indicatorElement = row.querySelector('.place-indicators');

      fragment.appendChild(row);
    });

    // Une seule manipulation DOM
    container.innerHTML = '';
    container.appendChild(fragment);
  },

  exposeDataToHighlightSystem: function() {
    // Enrichir chaque entrée de lieux avec une référence DOM directe
    const placeElements = document.querySelectorAll('.place-content');
    
    placeElements.forEach(element => {
      const placeName = element.dataset.place;
      if (placeName && lieux[placeName]) {
        // Référence bidirectionnelle
        lieux[placeName].domElement = element;
        element.dataset.placeData = JSON.stringify({
          c: lieux[placeName].c,
          events: Events.types.filter(t => lieux[placeName][Events.svgPrefix(t)])
        });
      }
    });
    
    console.log(`📌 ${placeElements.length} éléments DOM liés aux données de lieux`);
  },

  /**
   * Tri avec cache intelligent
   */
  getCachedSortedPlaces: function() {
    if (this.cache.sortedPlaces && this.cache.lastSortMode === sortMode) {
      return this.cache.sortedPlaces;
    }

    this.cache.sortedPlaces = this.getSortedPlaces();
    this.cache.lastSortMode = sortMode;
    return this.cache.sortedPlaces;
  },

  /**
   * Tri optimisé avec support des données natives
     */
  getSortedPlaces: function() {
    if (sortMode === 'alphabetical') {
      // Utilisation directe des données natives _main pour le regroupement
      const groups = new Map();

      Object.entries(lieux).forEach(([name, data]) => {
        if (data.isSubLocation && data.parentLocation) {
          // Sous-lieu : l'ajouter au groupe du parent
          if (!groups.has(data.parentLocation)) {
            groups.set(data.parentLocation, { main: null, subs: [] });
          }
          groups.get(data.parentLocation).subs.push([name, data]);
        } else {
          // Lieu principal
          if (!groups.has(name)) {
            groups.set(name, { main: null, subs: [] });
          }
          groups.get(name).main = [name, data];
        }
      });

      // Construire l'array trié
      const sorted = [];
      Array.from(groups.entries())
        .sort(([a], [b]) => a.localeCompare(b, 'fr', { sensitivity: 'base' }))
        .forEach(([groupName, group]) => {
          if (group.main) sorted.push(group.main);
          group.subs
            .sort(([a], [b]) => a.localeCompare(b, 'fr', { sensitivity: 'base' }))
            .forEach(sub => sorted.push(sub));
        });

      return sorted;
    }

    // Mode fréquence : simple tri par compteur
    return Object.entries(lieux).sort((a, b) => b[1].cnt - a[1].cnt);
  },

  /**
   * Création optimisée des éléments DOM
   */
  createPlaceRow: function(placeName, placeData, index) {
    const row = document.createElement('div');
    row.className = 'place-row';
    row.dataset.index = index;

    // Indicateurs d'événements
    const indicators = document.createElement('div');
    indicators.className = 'place-indicators';
    indicators.id = `indic-${index}`;

    // Contenu principal
    const content = document.createElement('div');
    content.className = 'place-content';
    content.id = `place-${index}`;
    content.dataset.place = placeName;
    content.dataset.placeClass = placeData.c || '';
    content.dataset.total = placeData.cnt || 0;

    // Construction du contenu interne
    content.innerHTML = this.buildPlaceContentHTML(placeName, placeData, index);

    row.appendChild(indicators);
    row.appendChild(content);

    return row;
  },

  /**
   * Construction optimisée du HTML interne
   */
  buildPlaceContentHTML: function(placeName, placeData, index) {
    const colorIndex = (index % 12) + 1;
    const displayName = this.getDisplayName(placeName, placeData);
    const eventItemsHTML = this.buildEventItemsHTML(placeData);

    return `
      <div class="place-left">
        <div class="place-color color-${colorIndex}"></div>
        <div class="place-name">${displayName}</div>
      </div>
      <div class="place-right">
        <div class="place-events">${eventItemsHTML}</div>
        <div class="place-count">${placeData.cnt || 0}</div>
      </div>
    `;
  },

  /**
   * Nom d'affichage selon le mode et les données natives
   */
  getDisplayName: function(placeName, placeData) {
    if (sortMode === 'alphabetical' && placeData.isSubLocation && placeData.subName) {
      return `<span class="sublocation-indicator">└ </span>${placeData.subName}`;
    }
    return placeName;
  },

  /**
   * Construction des indicateurs d'événements
   */
  buildEventItemsHTML: function(placeData) {
    return Events.types.map(eventType => {
      const count = placeData[Events.count(eventType)] || 0;
      const isActive = count > 0;

      return `
        <div class="event-item ${isActive ? 'active' : ''}" data-event="${eventType}">
          <span class="event-count">${count > 1 ? count : ''}</span>
          <span class="event-label">${Events.label(eventType)}</span>
        </div>
      `;
    }).join('');
  },

  /**
   * Mise à jour du résumé et support du survol des totaux
   */
  updateSummarySection: function() {
    // Générations
    const genElement = this.cache.elements.panel.querySelector('.generation-count');
    if (genElement) {
      const genLabel = window.FC_TRANSLATIONS?.[max_gen > 1 ? 'generations' : 'generation'] || 'génération';
      genElement.textContent = `${max_gen} ${genLabel}`;
    }

    // Lieux
    const placeCount = Object.keys(lieux).length;
    const placeLabel = window.FC_TRANSLATIONS?.[placeCount > 1 ? 'places' : 'place'] || 'lieu';
    this.cache.elements.summaryPlaces.textContent = `${placeCount} ${placeLabel}`;

    // Statistiques par événement
    const stats = LocationDataBuilder.getEventStatistics();

    Events.types.forEach((eventType, index) => {
      const countElement = this.cache.elements.summaryEventCounts[index];
      if (countElement) {
        const count = stats[eventType] || 0;
        countElement.textContent = count;

        // Préparation pour le futur surlignage NBMDS
        const labelElement = countElement.previousElementSibling;
        if (labelElement) {
          labelElement.dataset.eventType = eventType;
          labelElement.dataset.eventCount = count;
        }
      }
    });

    // Total
    const totalEvents = Object.values(stats).reduce((sum, count) => sum + count, 0);
    this.cache.elements.summaryTotal.textContent = totalEvents;

    // Personnes
    this.updatePersonsCounter();
  },

  /**
   * Compteur de personnes
   */
  updatePersonsCounter: function() {
    const personsElement = this.cache.elements.summaryPersons;
    if (!personsElement) return;

    // Utiliser le cache des ancêtres filtrés de LocationDataBuilder
    const filteredAncestors = LocationDataBuilder.filterAncestorsByGeneration(max_gen);

    let personsWithPlaces = 0;
    Object.values(filteredAncestors).forEach(person => {
      // Vérifier si au moins un lieu existe
      const hasPlace = Events.types.some(eventType => {
        const placeField = Events.place(eventType);
        return person[placeField]?.trim();
      });

      if (hasPlace) personsWithPlaces++;
    });

    personsElement.textContent = personsWithPlaces;
    const label = window.FC_TRANSLATIONS?.[personsWithPlaces > 1 ? 'persons' : 'person'] || 'personne';
    personsElement.title = `${personsWithPlaces} ${label} avec lieux`;
  },

  /**
   * Configuration des événements
   */
  setupNavigationListeners: function() {
    if (!this.cache.elements.placesList) return;

    // Les événements de surlignage sont maintenant gérés par IntegrationHelper

    // Conserver uniquement le clic pour la navigation
    this.cache.elements.placesList.addEventListener('click', (e) => {
      const placeContent = e.target.closest('.place-content');
      if (placeContent) {
        const placeName = placeContent.dataset.place;
        if (placeName && URLManager.navigateToPlace) {
          const newTab = e.ctrlKey || e.metaKey;
          URLManager.navigateToPlace(placeName, newTab);
        }
      }
    });
  },

  /**
   * Configuration du surlignage des totaux d'événements
   */
  setupEventTotalHighlights: function() {
    document.querySelectorAll('.summary-event-label').forEach(label => {
      const eventType = label.dataset.eventType;
      if (!eventType) return;

      label.style.cursor = 'pointer';

      label.addEventListener('mouseenter', () => {
        SVGHighlightHandler.highlightByEventType(eventType);
      });

      label.addEventListener('mouseleave', () => {
        SVGHighlightHandler.clearAllHighlights();
      });
    });
  },

  handlePlaceClick: function(placeName, event) {
    // Navigation vers la recherche de lieu
    if (placeName && URLManager.navigateToPlace) {
      const newTab = event.ctrlKey || event.metaKey;
      URLManager.navigateToPlace(placeName, newTab);
    }
  }
};

// ========== Interface utilisateur pour le panneau des lieux ==========
const PlacesPanelControls = {
  searchDebounceTimer: null, // Timers pour optimisation

  initialize: function() {
    if (!PlacesInterface.cache.elements.panel) {
      console.error('❌ PlacesInterface doit être initialisé avant PlacesPanelControls');
      return false;
    }

    this.initializeDefaultStates();
    this.setupEventListeners();
    return true;
  },

  /**
   * Configuration des listeners avec délégation
   */
  setupEventListeners: function() {
    const panel = PlacesInterface.cache.elements.panel;

    // Délégation pour tous les clics
    panel.addEventListener('click', (e) => {
      // Fermeture du panneau
      if (e.target.matches('.panel-close')) {
        e.preventDefault();
        this.togglePanel();
      }
      // Tri
      else if (e.target.closest('.sort-toggle')) {
        e.preventDefault();
        this.toggleSort();
      }
      // Événements
      else if (e.target.closest('.events-toggle')) {
        e.preventDefault();
        this.toggleEventsDisplay();
      }
      // Effacer recherche
      else if (e.target.matches('.search-clear')) {
        e.preventDefault();
        this.clearSearch();
      }
    });

    // Recherche avec debounce
    this.setupSearchListener();
  },

  /**
   * Recherche optimisée avec debounce
   */
  setupSearchListener: function() {
    const panel = PlacesInterface.cache.elements.panel;
    const searchInput = panel.querySelector('.search-input');
    if (!searchInput) return;

    searchInput.addEventListener('input', (e) => {
      clearTimeout(this.searchDebounceTimer);
      const query = e.target.value;

      // Recherche immédiate si effacement
      if (!query) {
        this.filterPlaces('');
        return;
      }

      // Debounce pour les autres cas
      this.searchDebounceTimer = setTimeout(() => {
        this.filterPlaces(query);
      }, 150);
    });
  },

  /**
   * États par défaut cohérents
   */
  initializeDefaultStates: function() {
    this.updateSortButtonIcon();

    const panel = PlacesInterface.cache.elements.panel;

    // État des événements
    if (showEvents) {
      panel.classList.add('show-events');
      const icon = document.querySelector('.events-toggle i');
      if (icon) icon.className = 'far fa-eye-slash';
    }

    // Recherche
    const searchInput = panel.querySelector('.search-input');
    if (searchInput) searchInput.value = '';
  },

  /**
   * Basculement du panneau
   */
  togglePanel: function() {
    const panel = PlacesInterface.cache.elements.panel;
    const isVisible = panel.style.display !== 'none';
    panel.style.display = isVisible ? 'none' : 'block';
  },

  /**
   * Basculement du tri
   */
  toggleSort: function() {
    sortMode = sortMode === 'frequency' ? 'alphabetical' : 'frequency';

    PlacesInterface.cache.invalidateSort();
    this.updateSortButtonIcon();
    PlacesInterface.generatePlacesList();
    URLManager.updateCurrentURL();
  },

  /**
   * Mise à jour de l'icône de tri
   */
  updateSortButtonIcon: function() {
    const icon = document.querySelector('.sort-toggle i');
    if (icon) {
      icon.className = sortMode === 'alphabetical'
        ? 'fas fa-arrow-down-wide-short'
        : 'fas fa-arrow-down-a-z';
    }

    const button = document.querySelector('.sort-toggle');
    if (button) {
      const key = sortMode === 'alphabetical' ? 'sort_alphabetically' : 'sort_by_frequency';
      button.title = window.FC_TRANSLATIONS?.[key] || 'Changer le tri';
    }
  },

  /**
   * Basculement de l'affichage détaillé
   */
  toggleEventsDisplay: function() {
    showEvents = !showEvents;

    const panel = PlacesInterface.cache.elements.panel;
    panel.classList.toggle('show-events');

    const icon = document.querySelector('.events-toggle i');
    if (icon) {
      icon.className = panel.classList.contains('show-events')
        ? 'far fa-eye-slash'
        : 'far fa-eye';
    }

    URLManager.updateCurrentURL();
  },

  /**
   * Filtrage optimisé
   */
  filterPlaces: function(query) {
    const normalizedQuery = query.toLowerCase().trim();

    requestAnimationFrame(() => {
      const rows = document.querySelectorAll('.place-row');
      let visibleCount = 0;
      let totalCount = rows.length;

      if (!normalizedQuery) {
        // Réafficher tout rapidement
        rows.forEach(row => {
          row.style.display = '';
        });
        visibleCount = totalCount;
      } else {
        // Filtrer
        rows.forEach(row => {
          const nameEl = row.querySelector('.place-name');
          const placeName = nameEl ? nameEl.textContent.toLowerCase() : '';
          const matches = placeName.includes(normalizedQuery);

          row.style.display = matches ? '' : 'none';
          if (matches) visibleCount++;
        });
      }

      this.updateClearButtonVisibility(normalizedQuery);
      this.updateSearchResultsCount(visibleCount, totalCount);
    });
  },

  /**
   * Affichage du compteur de résultats
   */
  updateSearchResultsCount: function(visible, total) {
    let counter = document.querySelector('.search-results-count');

    if (visible < total) {
      if (!counter) {
        counter = document.createElement('div');
        counter.className = 'search-results-count';
        counter.style.cssText = 'font-size: 10px; color: #666; margin-top: 2px;';
        const section = document.querySelector('.controls-search-section');
        if (section) section.appendChild(counter);
      }
      counter.textContent = `${visible}/${total}`;
      counter.style.display = 'block';
    } else if (counter) {
      counter.style.display = 'none';
    }
  },

  /**
   * Visibilité du bouton clear
   */
  updateClearButtonVisibility: function(query) {
    const clearBtn = document.querySelector('.search-clear');
    if (clearBtn) {
      clearBtn.style.display = query ? 'block' : 'none';
    }
  },

  /**
   * Effacement de la recherche
   */
  clearSearch: function() {
    const input = document.querySelector('.search-input');
    if (input) {
      input.value = '';
      input.focus();
    }

    this.filterPlaces('');
  }
};

// ========== Module de rendu circulaire ==========
const CircularModeRenderer = {
  // Active/désactive le mode circulaire
  toggle: function() {
    isCircularMode = !isCircularMode;

    const btn = document.getElementById('b-circular-mode');
    if (btn) {
      btn.classList.toggle('active', isCircularMode);
      btn.title = isCircularMode ? 'Revenir au mode éventail' : 'Mode circulaire (360°)';
    }

    // Désactiver/activer les boutons d'angle au lieu de les masquer
    CONFIG.available_angles.forEach(angle => {
      const angleBtn = document.getElementById(`b-angle-${angle}`);
      if (angleBtn) {
        angleBtn.classList.toggle('disabled', isCircularMode);
        angleBtn.disabled = isCircularMode;
      }
    });

    // Mettre à jour l'URL
    URLManager.updateCurrentURL();

    // Redessiner
    FanchartApp.calculateDimensions();
    FanchartApp.reRenderWithCurrentGenerations();
  },

  /**
   * Rend le centre en mode couple (S2 au nord, S3 au sud)
   */
  renderCoupleCenter: function() {
    const centerGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
    centerGroup.setAttribute("id", "couple-center");
    fanchart.appendChild(centerGroup);

    const s2 = ancestor["S2"]; // Père
    const s3 = ancestor["S3"]; // Mère
    const r = CONFIG.a_r[0];

    // Groupes individuels pour chaque parent,
    // facilite la gestion des événements et du surlignage —
    // Groupe pour le père (demi-cercle nord)
    if (s2) {
        const s2Group = document.createElementNS("http://www.w3.org/2000/svg", "g");
        s2Group.setAttribute("id", "couple-s2");
        centerGroup.appendChild(s2Group);

        // Secteur de fond
        SVGRenderer.drawPie(s2Group, 0, r, -90, 90, s2,
          { type: 'person', isBackground: true });

        // Texte centré dans le demi-cercle nord
        const text2 = document.createElementNS("http://www.w3.org/2000/svg", "text");
        text2.setAttribute("x", center_x);
        text2.setAttribute("y", center_y - r/3);
        text2.setAttribute("text-anchor", "middle");
        text2.setAttribute("class", "couple-text");
        text2.innerHTML = `<tspan>${s2.fn}</tspan><tspan x="${center_x}" dy="15">${s2.sn}</tspan>`;
        s2Group.appendChild(text2);

        // Secteur interactif (doit être en dernier pour capturer les événements)
        SVGRenderer.drawPie(s2Group, 0, r, -90, 90, s2, { type: 'person' });
      }

      // Groupe pour la mère (demi-cercle sud)
      if (s3) {
        const s3Group = document.createElementNS("http://www.w3.org/2000/svg", "g");
        s3Group.setAttribute("id", "couple-s3");
        centerGroup.appendChild(s3Group);

        // Secteur de fond
        SVGRenderer.drawPie(s3Group, 0, r, 90, 270, s3,
          { type: 'person', isBackground: true });

        // Texte centré dans le demi-cercle sud
        const text3 = document.createElementNS("http://www.w3.org/2000/svg", "text");
        text3.setAttribute("x", center_x);
        text3.setAttribute("y", center_y + r/3);
        text3.setAttribute("text-anchor", "middle");
        text3.setAttribute("class", "couple-text");
        text3.innerHTML = `<tspan>${s3.fn}</tspan><tspan x="${center_x}" dy="15">${s3.sn}</tspan>`;
        s3Group.appendChild(text3);

        // Secteur interactif
        SVGRenderer.drawPie(s3Group, 0, r, 90, 270, s3, { type: 'person' });
      }

      // Ligne de séparation élégante
      const separator = document.createElementNS("http://www.w3.org/2000/svg", "line");
      separator.setAttribute("x1", center_x - r);
      separator.setAttribute("y1", center_y);
      separator.setAttribute("x2", center_x + r);
      separator.setAttribute("y2", center_y);
      separator.setAttribute("stroke", "#ccc");
      separator.setAttribute("stroke-width", "1");
      separator.setAttribute("stroke-dasharray", "3,3"); // Ligne pointillée pour plus d'élégance
      centerGroup.appendChild(separator);
      /*
      // Optionnel : Ajouter un petit texte pour le S1 au centre
      if (ancestor["S1"]) {
        const s1Text = document.createElementNS("http://www.w3.org/2000/svg", "text");
        s1Text.setAttribute("x", center_x);
        s1Text.setAttribute("y", center_y);
        s1Text.setAttribute("text-anchor", "middle");
        s1Text.setAttribute("class", "s1-indicator");
        s1Text.setAttribute("font-size", "10");
        s1Text.setAttribute("fill", "#666");
        s1Text.textContent = "⬤"; // Point central discret
        const s1Title = document.createElementNS("http://www.w3.org/2000/svg", "title");
        s1Title.textContent = `${ancestor["S1"].fn} ${ancestor["S1"].sn} (enfant du couple)`;
        s1Text.appendChild(s1Title);
        centerGroup.appendChild(s1Text);
      }*/
    },

  /**
   * Décale une branche d'ancêtres pour qu'un parent devienne S1
   */
  shiftAncestorsForParent: function(originalAncestors, parentSosa) {
    const shifted = {};

    // Le parent devient S1 pour le rendu
    shifted["S1"] = originalAncestors["S" + parentSosa];

    // Fonction récursive pour décaler toute la branche
    const shiftBranch = (oldSosa, newSosa) => {
      const person = originalAncestors["S" + oldSosa];
      if (person) {
        shifted["S" + newSosa] = person;
        // Décaler récursivement les parents
        shiftBranch(oldSosa * 2, newSosa * 2);       // Père
        shiftBranch(oldSosa * 2 + 1, newSosa * 2 + 1); // Mère
      }
    };

    // Décaler les grands-parents
    shiftBranch(parentSosa * 2, 2);
    shiftBranch(parentSosa * 2 + 1, 3);

    return shifted;
  }
};

// ========== Fonctions utilitaires pour la géométrie ==========
function polarToCartesian(r, angle) {
  const rad = Math.PI / 180 * angle;
  return {
    x: center_x + r * Math.cos(rad),
    y: center_y + r * Math.sin(rad)
  };
}

// ========== Rendu SVG ==========
const SVGRenderer = {
  drawContour: function(g, r1, r2, a1, a2) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a1);
    const p2 = polarToCartesian(r2, a2);
    const p3 = polarToCartesian(r1, a2);
    const p4 = polarToCartesian(r1, a1);

    path.setAttribute("d",
      `M ${p1.x},${p1.y} ` +
      `A ${r2} ${r2} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y} ` +
      `L ${p3.x},${p3.y} ` +
      `A ${r1} ${r1} 0 ${(a2 - a1 > 180 ? 1 : 0)} 0 ${p4.x},${p4.y} Z`
    );
    path.setAttribute("class", "contour");
    g.append(path);
  },

  drawRadialLine: function(g, r1, r2, a) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a);
    const p2 = polarToCartesian(r1, a);

    path.setAttribute("d", `M ${p1.x},${p1.y} L ${p2.x},${p2.y}`);
    path.setAttribute("class", "middle");
    g.append(path);
  },

  drawCircle: function(g, r, cx, cy, p, options = {}) {
    const circle = document.createElementNS("http://www.w3.org/2000/svg", "circle");
    circle.setAttribute("cx", cx);
    circle.setAttribute("cy", cy);
    circle.setAttribute("r", r);

    if (options.isBackground) {
      // Version background - applique les classes pour lieux et âge
      let classes = ['bg'];

      if (p.birth_place && lieux[p.birth_place]) {
        classes.push(`bi-${lieux[p.birth_place].c}`);
      }
      if (p.baptism_place && lieux[p.baptism_place]) {
        classes.push(`ba-${lieux[p.baptism_place].c}`);
      }
      if (p.death_place && lieux[p.death_place]) {
        classes.push(`de-${lieux[p.death_place].c}`);
      }
      if (p.burial_place && lieux[p.burial_place]) {
        classes.push(`bu-${lieux[p.burial_place].c}`);
      }
      if (p.death_age) {
        classes.push(Utils.deathAgeClass(p.death_age));
      }

      circle.setAttribute("class", classes.join(' '));
    } else {
      // Version interactive
      circle.setAttribute("class", "link");

      // Titre
      const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
      const age = (p.death_age && p.death_age !== "" && !isNaN(parseInt(p.death_age)))
        ? ` (${p.death_age} ans)`
        : "";
      title.textContent = `(Sosa 1) ${p.fn} ${p.sn}${age}\nCtrl+clic pour la fiche individuelle`;
      circle.appendChild(title);

      // Événements - réutilisation des méthodes universelles
      circle.onclick = (e) => this.handleClick(e, p);
      circle.onmouseenter = (e) => {
        e.stopPropagation();
        // Panneau d'information
        const panel = document.getElementById("person-panel");
        if (panel) {
          this.buildTooltipContent(panel, p, 'person');
          panel.style.display = "block";
        }
        // Nouveau système de surlignage
        HighlightCoordinator.handleSectorHover(circle, p, 'person', e);
      };
      circle.onmouseleave = (e) => {
        e.stopPropagation();
        // Panneau d'information
        const panel = document.getElementById("person-panel");
        if (panel) {
          panel.style.display = "none";
          panel.innerHTML = "";
        }
        // Nouveau système de surlignage
        HighlightCoordinator.handleSectorLeave(p, 'person');
      };
    }

    g.append(circle);
    return circle;
  },

  drawPie: function(g, r1, r2, a1, a2, p, options = {}) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    const p1 = polarToCartesian(r2, a1);
    const p2 = polarToCartesian(r2, a2);
    const p3 = polarToCartesian(r1, a2);
    const p4 = polarToCartesian(r1, a1);

    path.setAttribute("d",
      `M ${p1.x},${p1.y} ` +
      `A ${r2} ${r2} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y} ` +
      `L ${p3.x},${p3.y} ` +
      `A ${r1} ${r1} 0 ${(a2 - a1 > 180 ? 1 : 0)} 0 ${p4.x},${p4.y} Z`
    );

    // NOUVEAU : Toujours construire les classes de base
    let baseClasses = options.isBackground ? ['bg'] : ['link'];
    
    // NOUVEAU : Toujours ajouter les classes de lieu
    if (p) {
      if (options.type === 'person') {
        Events.types.forEach(eventType => {
          const placeField = Events.place(eventType);
          const svgPrefix = Events.svgPrefix(eventType);
          if (p[placeField] && lieux[p[placeField]]) {
            baseClasses.push(`${svgPrefix}-${lieux[p[placeField]].c}`);
          }
        });
        if (p.death_age) baseClasses.push(Utils.deathAgeClass(p.death_age));
      } else if (options.type === 'marriage') {
        if (p.marriage_place && lieux[p.marriage_place]) {
          baseClasses.push(`ma-${lieux[p.marriage_place].c}`);
        }
        if (p.marriage_length) {
          const marriageClass = Utils.marriageLengthClass(p.marriage_length);
          if (marriageClass) baseClasses.push(marriageClass);
        }
      }
    }
    
    path.setAttribute("class", baseClasses.join(' '));
    
    // Ajouter les fonctionnalités interactives si nécessaire
    if (!options.isBackground) {
      this.applyInteractiveFeatures(path, p, options.type);
    }

    g.append(path);
    return path;
  },

  applyBackgroundClasses: function(element, p, type) {
    let classes = ['bg'];

    if (type === 'person') {
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        const svgPrefix = Events.svgPrefix(eventType); // 'bi', 'ba', 'ma', 'de', 'bu'

        if (p[placeField] && lieux[p[placeField]]) {
          classes.push(`${svgPrefix}-${lieux[p[placeField]].c}`);
        }
      });

      if (p.death_age) classes.push(Utils.deathAgeClass(p.death_age));

    } else if (type === 'marriage') {
      const marriagePlaceField = Events.place('marriage');
      if (p[marriagePlaceField] && lieux[p[marriagePlaceField]]) {
        const svgPrefix = Events.svgPrefix('marriage'); // 'ma'
        classes.push(`${svgPrefix}-${lieux[p[marriagePlaceField]].c}`);
      }

      if (p.marriage_length) {
        const marriageClass = Utils.marriageLengthClass(p.marriage_length);
        if (marriageClass) classes.push(marriageClass);
      }
    }

    element.setAttribute("class", classes.join(' '));
  },

  applyInteractiveFeatures: function(element, p, type) {
    // Vérifier que l'élément est valide pour l'interaction
    if (!p || (p.fn === "?" || (!p.fn && !p.sosasame && type !== 'marriage'))) return;

    // Note: la classe "link" est déjà appliquée dans drawPie/drawCircle
    // Nous ajoutons seulement les événements et le titre
    
    const panel = document.getElementById("person-panel");

    // Gestion du clic
    element.addEventListener("click", (e) => {
      e.stopPropagation();
      this.handleClick(e, p);
    });

    // Gestion du survol
    element.addEventListener("mouseenter", (e) => {
      e.stopPropagation();

      // Panneau d'information
      if (panel) {
        this.buildTooltipContent(panel, p, type);
        panel.style.display = "block";
      }

      // Nouveau système de surlignage
      HighlightCoordinator.handleSectorHover(element, p, type, e);
    });

    // Gestion de la sortie
    element.addEventListener("mouseleave", (e) => {
      e.stopPropagation();

      // Panneau d'information
      if (panel) {
        panel.style.display = "none";
        panel.innerHTML = "";
      }

      // Nouveau système de surlignage
      HighlightCoordinator.handleSectorLeave(p, type);
    });
  },

  buildTooltipContent: function(panel, p, type) {
    if (type === "person") {
      panel.innerHTML = `
        <h2>${p.fn} ${p.sn}</h2>
        <div class="subtitle">${p.dates}${p.death_age && !isNaN(parseInt(p.death_age)) ? ` ${p.death_age} ans` : ""}</div>
        ${p.birth_place ? `<div><strong>Naissance :</strong> ${p.birth_place}</div>` : ""}
        ${p.death_place ? `<div><strong>Décès :</strong> ${p.death_place}</div>` : ""}
      `;
    } else if (type === "marriage") {
      const years = parseInt(p.marriage_length) || -1;
      panel.innerHTML = `
        <h2>Mariage</h2>
        ${p.marriage_date ? `<div><strong>Date :</strong> ${p.marriage_date}</div>` : ""}
        ${p.marriage_place ? `<div><strong>Lieu :</strong> ${p.marriage_place}</div>` : ""}
        ${years >= 0 ? `<div><strong>Durée :</strong> ${years} ${years === 1 ? "an" : "ans"}</div>` : ""}
      `;
    }
  },

  handleClick: function(e, person) {
    const sortToggle = e.target.closest('#b-sort-places');
      if (sortToggle) {
      e.preventDefault();
      UIManager.toggleSort();
      return;
    }
    // Pour la navigation vers les fiches, exiger Ctrl/Cmd
    if (!e.ctrlKey && !e.metaKey) {
      // Pas de navigation sans modificateur
      return;
    }
    if (!link_to_person) {
      alert("Erreur: Impossible d'accéder à la fiche individuelle");
      return;
    }
    const li = e.target.closest('li[data-location]');

    // Clic sur une personne (secteur du fanchart)
    if (person && person.fnk && person.snk) {
      URLManager.navigateToPerson(person, true, false);
      return;
    }

    // Clic sur un lieu en mode wizard
    if (li && document.body.dataset.wizard === "1") {
      e.preventDefault();
      const placeName = li.dataset.location;
      URLManager.navigateToPlace(placeName, true);
      return;
    }
  },

  drawSectorText: function(pg, r1, r2, a1, a2, sosa, p, classes, generation, isSame = false) {
    let mode;

    if (CONFIG.a_m[generation - 1] === "C3") {
      mode = 'C3';
    } else if (CONFIG.a_m[generation - 1] === "R3" && !isSame) {
      mode = 'R3';
    } else if (CONFIG.a_m[generation - 1] === "R2" && !isSame) {
      mode = 'R2';
    } else if (CONFIG.a_m[generation - 1] === "R1" || isSame) {
      mode = 'R1';
    }

    return T.drawText(pg, mode, {
      r1: r1 + 10,
      r2: r2,
      a1: a1,
      a2: a2,
      sosa: sosa,
      p: p,
      classes: classes
    });
  },

  drawNavigationSymbol: function(g, pathId, p, pathLength, hasParents) {
    let fontSize = 80;
    if (2 * standard_width > pathLength) {
      fontSize = Math.round(100 * pathLength / 2 / standard_width);
    }
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    if (hasParents) {
      text.setAttribute("class", "link icon");
      text.innerHTML = `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${fontSize}%;">&#x25B2;</textPath>`;
      text.onclick = (e) => {
        const useNewTab = e.ctrlKey || e.metaKey;
        URLManager.navigateToPerson(p, useNewTab, true);
      };
    } else {
      text.setAttribute("class", "no-link");
      text.innerHTML = `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${fontSize}%;">&#x2716;</textPath>`;
    }
    const title = document.createElementNS("http://www.w3.org/2000/svg", "title");
    title.textContent = hasParents ? `Recentrer l'arbre sur ${p.fn} ${p.sn}` : `${p.fn} ${p.sn} : aucun parent connu`;
    text.appendChild(title);
    g.append(text);
    return text;
  },

  drawParentIndicator: function(g, r, a1, a2, sosa, p) {
    if (!p || p.fn === "?" || p.fn === "" || !p.fn) {
      return; // Pas d'icône du tout
    }
    const pathLength = T.createCircularPath(g, `tpiS${sosa}`, r, a1, a2);
    return this.drawNavigationSymbol(g, `tpiS${sosa}`, p, pathLength, p.has_parents);
  }
};

// ========== Système de rendu de texte unifié ==========
const TextRenderer = {
  _bboxCache: {},

  getBBoxCached: function(textContent) {
    if (!this._bboxCache[textContent]) {
      standard.textContent = textContent;
      this._bboxCache[textContent] = standard.getBBox();
    }
    return this._bboxCache[textContent];
  },

  drawText: function(g, mode, params) {
    // Construire les classes CSS pour les lieux
    const textClasses = this.buildLocationClasses(params.p, params.classes || "");

    switch(mode) {
      case 'S1':
        return this.drawCentralText(g, params.x, params.y, params.p, textClasses);

      case 'C3':
        return this.drawCircularText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses);

      case 'R3':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 3);

      case 'R2':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 2);

      case 'R1':
        return this.drawRadialText(g, params.r1, params.r2, params.a1, params.a2, params.sosa, params.p, textClasses, 1);

      default:
        console.warn(`Mode de texte non reconnu: ${mode}`);
        return null;
    }
  },

  buildLocationClasses: function(p, baseClasses) {
    let classes = baseClasses;

    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType);
      const svgPrefix = Events.svgPrefix(eventType);

      if (p[placeField] && lieux[p[placeField]]) {
        classes += ` ${svgPrefix}-t${lieux[p[placeField]].c}`;
      }
    });

    return classes.trim();
  },

  drawCentralText: function(g, x, y, p, classes) {
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("x", x);
    text.setAttribute("y", y);
    text.setAttribute("class", classes);

    // Calcul des tailles de police adaptatives pour éviter le débordement
    const fontSizes = this.calculateAdaptiveFontSizes([p.fn, p.sn]);

    // Construction du texte avec tailles adaptatives
    text.innerHTML =
      `<tspan style="font-size:${fontSizes[0]}%">${p.fn}</tspan>` +
      `<tspan x="${x}" dy="15" style="font-size:${fontSizes[1]}%">${p.sn}</tspan>` +
      `<tspan class="dates" x="${x}" dy="15">${p.dates}</tspan>`;

    g.append(text);
    return text;
  },

  drawCircularText: function(g, r1, r2, a1, a2, sosa, p, classes) {
    const height = Math.abs(r2 - r1) / 3;

    // Trois arcs concentriques pour prénom, nom, dates
    const pathLength1 = this.createCircularPath(g, `tp1S${sosa}`, (r2-r1)*3/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height);

    const pathLength2 = this.createCircularPath(g, `tp2S${sosa}`, (r2-r1)*2/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height);

    const pathLength3 = this.createCircularPath(g, `tp3S${sosa}`, (r2-r1)/4 + r1, a1, a2);
    this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height);

    return g;
  },

  drawRadialText: function(g, r1, r2, a1, a2, sosa, p, classes, lineCount) {
    // Calcul des paramètres de direction selon l'orientation
    const params = this.calculateRadialParameters(r1, r2, a1, a2, lineCount);
    const height = Math.abs(a2 - a1) / 360 * 2 * Math.PI * r1 / lineCount;

    if (lineCount === 3) {
      // Trois lignes : prénom, nom, dates
      const pathLength1 = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, p.fn, classes, pathLength1, height);

      const pathLength2 = this.createRadialPath(g, `tp2S${sosa}`, params.r1, params.r2, params.angles[1]);
      this.placeTextOnPath(g, `tp2S${sosa}`, p.sn, classes, pathLength2, height);

      const pathLength3 = this.createRadialPath(g, `tp3S${sosa}`, params.r1, params.r2, params.angles[2]);
      this.placeTextOnPath(g, `tp3S${sosa}`, p.dates, classes + " dates", pathLength3, height);

    } else if (lineCount === 2) {
      // Deux lignes : nom complet, dates
      const pathLength1 = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, `${p.fn} ${p.sn}`, classes, pathLength1, height);

      const pathLength2 = this.createRadialPath(g, `tp2S${sosa}`, params.r1, params.r2, params.angles[1]);
      this.placeTextOnPath(g, `tp2S${sosa}`, p.dates, classes + " dates", pathLength2, height);

    } else { // lineCount === 1
      // Une ligne : nom complet seulement
      const pathLength = this.createRadialPath(g, `tp1S${sosa}`, params.r1, params.r2, params.angles[0]);
      this.placeTextOnPath(g, `tp1S${sosa}`, `${p.fn} ${p.sn}`, classes, pathLength, height);
    }

    return g;
  },

  calculateRadialParameters: function(r1, r2, a1, a2, lineCount) {
    let myR1, myR2, angles = [];

    if (a1 >= -90) {
      // Orientation normale
      myR1 = r1;
      myR2 = r2;

      if (lineCount === 3) {
        angles = [
          a2 - (a2-a1)*3/4,  // Position 3/4
          a2 - (a2-a1)*2/4,  // Position 1/2
          a2 - (a2-a1)/4     // Position 1/4
        ];
      } else if (lineCount === 2) {
        angles = [
          a2 - (a2-a1)*2/3,  // Position 2/3
          a2 - (a2-a1)/3     // Position 1/3
        ];
      } else {
        angles = [a2 - (a2-a1)/2]; // Position centrale
      }
    } else {
      // Orientation inversée
      myR1 = r2;
      myR2 = r1;

      if (lineCount === 3) {
        angles = [
          a1 + (a2-a1)*3/4,
          a1 + (a2-a1)*2/4,
          a1 + (a2-a1)/4
        ];
      } else if (lineCount === 2) {
        angles = [
          a1 + (a2-a1)*2/3,
          a1 + (a2-a1)/3
        ];
      } else {
        angles = [a1 + (a2-a1)/2];
      }
    }

    return { r1: myR1, r2: myR2, angles: angles };
  },

  createCircularPath: function(g, id, r, a1, a2) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    const p1 = polarToCartesian(r, a1);
    const p2 = polarToCartesian(r, a2);
    path.setAttribute("d",
      `M ${p1.x},${p1.y} A ${r} ${r} 0 ${(a2 - a1 > 180 ? 1 : 0)} 1 ${p2.x},${p2.y}`
    );
    path.setAttribute("id", id);
    g.append(path);

    // Retourne la longueur approximative du chemin
    return Math.abs(a2 - a1) / 360 * 2 * Math.PI * r;
  },

  createRadialPath: function(g, id, r1, r2, a) {
    const path = document.createElementNS("http://www.w3.org/2000/svg", "path");
    path.setAttribute("class", "none");
    const p1 = polarToCartesian(r1, a);
    const p2 = polarToCartesian(r2, a);
    path.setAttribute("d", `M ${p1.x},${p1.y} L ${p2.x},${p2.y}`);
    path.setAttribute("id", id);
    g.append(path);

    // Retourne la longueur du chemin
    return Math.abs(r2 - r1);
  },

  placeTextOnPath: function(g, pathId, textContent, classes, pathLength, pathHeight) {
    const bbox = this.getBBoxCached(textContent);
    const textWidth = bbox.width;
    const textHeight = bbox.height;

    let fontSizeByWidth = 100;
    if (textWidth > pathLength * CONFIG.security) {
      fontSizeByWidth = Math.round(100 * pathLength * CONFIG.security / textWidth);
    }

    let fontSizeByHeight = 100;
    if (textHeight > pathHeight * CONFIG.security) {
      fontSizeByHeight = Math.round(100 * pathHeight * CONFIG.security / textHeight);
    }

    const finalFontSize = Math.min(fontSizeByWidth, fontSizeByHeight);

    // Création de l'élément text avec textPath
    const text = document.createElementNS("http://www.w3.org/2000/svg", "text");
    text.setAttribute("class", "text " + classes);
    text.innerHTML =
      `<textPath xlink:href="#${pathId}" startOffset="50%" style="font-size:${finalFontSize}%;">` +
      textContent +
      `</textPath>`;

    g.append(text);
    return text;
  },

  calculateAdaptiveFontSizes: function(texts) {
    const textReductionFactor = CONFIG.text_reduction_factor || 0.9;
    const maxWidth = 2 * CONFIG.a_r[0] * CONFIG.security;

    return texts.map(text => {
      const bbox = this.getBBoxCached(text);
      const width = bbox.width;

      if (width > maxWidth) {
        return Math.round(100 * maxWidth / width * textReductionFactor);
      } else {
        return Math.round(100 * textReductionFactor);
      }
    });
  },

  drawMarriageDate: function(g, sosa, r, a1, a2, marriageDate, classes) {
    const pathId = "pmS" + sosa;

    // Réutilise createCircularPath qui remplace path1
    const pathLength = T.createCircularPath(g, pathId, r, a1, a2);

    // Réutilise placeTextOnPath qui remplace text2
    return this.placeTextOnPath(g, pathId, marriageDate, classes, pathLength, 10);
  }
};

// ========== Interface utilisateur ==========
const UIManager = {
  addNavigationHelp: function() {
    var helpPanel = document.createElement('div');
    helpPanel.id = 'navigation-help';
    helpPanel.style.display = 'none'; // Caché par défaut
    helpPanel.innerHTML = `
      <div class="help-title">💡 Aide Navigation</div>
      <div><strong>Souris :</strong></div>
      <div>– Glisser : déplacer l'arbre</div>
      <div>– Molette : zoomer</div>
      <div>– Survol : voir les détails</div>
      <div><strong>Raccourcis :</strong></div>
      <div>– <kbd>Ctrl</kbd>+clic : fiche individuelle</div>
      <div>– ▲ : navigation sur ancêtre</div>
      <div style="margin-top: 8px; text-align: center;">
      </div>
    `;
    document.body.appendChild(helpPanel);
  },
};

const ColorManager = {
  EVENT_TYPES: ["bi", "ba", "ma", "de", "bu"],

  createLocationStyles: function(index, c_h, c_l) {
    const root = document.documentElement;
    const sheet = [...document.styleSheets].find(s => s.title === "fc-auto");

    root.style.setProperty('--fc-color-' + index, 'hsl(' + c_h + ',100%,' + c_l + '%)');

    // Créer les règles CSS pour les secteurs SVG
    const eventTypes = ['bi', 'ba', 'ma', 'de', 'bu'];
    eventTypes.forEach(eventType => {
      sheet.insertRule(
        `body.place_color svg .${eventType}-L${index} { fill: var(--fc-color-${index}); }`,
        sheet.cssRules.length
      );
    });
    // Règle générique sans préfixe
    sheet.insertRule(
      `body.place_color svg .L${index} { fill: var(--fc-color-${index}); }`,
      sheet.cssRules.length
    );
  },

  setColorMode: function(newMode) {
    // Nettoyer l'état précédent
    document.body.classList.remove('place_color', 'death-age');

    // Désactiver tous les toggles NMBDS
    this.EVENT_TYPES.forEach(id => {
      const checkbox = document.getElementById(id);
      if (checkbox) checkbox.checked = false;
    });

    // Appliquer le nouveau mode
    if (newMode === 'place_color') {
      document.body.className = "place_color";
      tool = "place_color";
      // Activer M par défaut
      const maCheckbox = document.getElementById("ma");
      if (maCheckbox) maCheckbox.checked = true;
      this.applyColorization();
    } else if (newMode === 'death-age') {
      document.body.className = "death-age";
      tool = "death-age";
    } else {
      document.body.className = "";
      tool = "";
    }

    // Mettre à jour la visibilité des contrôles
    this.updateControlsVisibility();

    // Mettre à jour l'état visuel des boutons
    this.updateButtonStates();

    // Synchroniser l'URL
    URLManager.updateCurrentURL();
  },

  updateControlsVisibility: function() {
    const isPlaceColorActive = document.body.classList.contains('place_color');

    // Event toggles NMBDS
    const eventToggles = document.querySelector('.event-toggles');
    if (eventToggles) {
      eventToggles.style.display = isPlaceColorActive ? 'flex' : 'none';
    }

    // Bouton de tri
    const sortButton = document.getElementById("b-sort-places");
    if (sortButton) {
      sortButton.style.display = isPlaceColorActive ? 'inline-flex' : 'none';
    }
  },

  updateButtonStates: function() {
    // Tous les boutons utilisent la même classe .active
    const ageButton = document.getElementById("b-death-age");
    const placesButton = document.getElementById("b-places-colorise");
    const sortButton = document.getElementById("b-sort-places");

    if (ageButton) ageButton.classList.toggle("active", tool === "death-age");
    if (placesButton) placesButton.classList.toggle("active", tool === "place_color");
    if (sortButton) sortButton.classList.toggle("active", sortMode === "alphabetical");
  },

  applyColorization: function() {
    const fanchart = document.getElementById("fanchart");

    Events.types.forEach(eventType => {
      const svgPrefix = Events.svgPrefix(eventType);
      const checkbox = document.getElementById(svgPrefix);
      const isChecked = checkbox ? checkbox.checked : false;

      fanchart.classList.toggle(svgPrefix, isChecked);
    });

    URLManager.updateCurrentURL();
  },

  initializeColorEvents: function() {
    // Événements des checkboxes NMBDS
    Events.types.forEach(eventType => {
      const svgPrefix = Events.svgPrefix(eventType); // 'bi', 'ba', 'ma', 'de', 'bu'
      const checkbox = document.getElementById(svgPrefix);
      if (checkbox) {
        checkbox.onclick = this.applyColorization.bind(this);
      }
    });

    document.getElementById("b-circular-mode").onclick = () => CircularModeRenderer.toggle();

    // Bouton colorisation lieux
    document.getElementById("b-places-colorise").onclick = function() {
      const isActive = document.body.classList.contains("place_color");

      if (isActive) {
        // Désactiver
        document.body.className = "";
        tool = "";
        this.classList.remove("active");

        // Tout désactiver
        ColorManager.EVENT_TYPES.forEach(id => {
          document.getElementById(id).checked = false;
        });
      } else {
        // Activer avec M par défaut uniquement
        document.body.className = "place_color";
        tool = "place_color";
        this.classList.add("active");

        // Désactiver death-age si actif
        const ageButton = document.getElementById("b-death-age");
        if (ageButton) ageButton.classList.remove("active");

        // Activer seulement M
        document.getElementById("bi").checked = false;
        document.getElementById("ba").checked = false;
        document.getElementById("ma").checked = true;
        document.getElementById("de").checked = false;
        document.getElementById("bu").checked = false;
      }

      // Appliquer la colorisation
      ColorManager.applyColorization();
      ColorManager.updateControlsVisibility();
      URLManager.updateCurrentURL();
    };

    // Bouton âges (exclusion mutuelle)
    document.getElementById("b-death-age").onclick = function() {
      const isActive = document.body.classList.contains("death-age");

      if (isActive) {
        // Désactiver complètement
        document.body.className = "";
        tool = "";
        this.classList.remove("active");
      } else {
        document.body.className = "death-age";
        tool = "death-age";
        this.classList.add("active");

        // Désactiver colorisation lieux si active
        const placesButton = document.getElementById("b-places-colorise");
        if (placesButton) placesButton.classList.remove("active");
      }

      URLManager.updateCurrentURL();
    };

    // Masquer les contrôles au démarrage si nécessaire
    if (!has_ba) {
      const baLabel = document.getElementById("bal");
      if (baLabel) baLabel.style.display = "none";
    }
    if (!has_bu) {
      const buLabel = document.getElementById("bul");
      if (buLabel) buLabel.style.display = "none";
    }
  }
};

const AngleManager = {
  // Obtenir l'angle actuel
  getCurrentAngle: function() {
    return current_angle;
  },

  // Changer l'angle et redessiner
  setAngle: function(newAngle) {
    if (!CONFIG.available_angles.includes(newAngle)) {
      console.warn(`Angle ${newAngle} non supporté`);
      return;
    }

    if (newAngle === current_angle) {
      return;
    }

    current_angle = newAngle;

    // Mettre à jour l'URL avec le système existant
    URLManager.updateCurrentURL();

    // Mettre à jour l'interface
    this.updateAngleButtons();

    // Redessiner le graphique
    FanchartApp.reRenderWithCurrentGenerations();
  },

  // Mettre à jour l'état visuel des boutons
  updateAngleButtons: function() {
    CONFIG.available_angles.forEach(angle => {
      const btn = document.getElementById(`b-angle-${angle}`);
      if (btn) {
        btn.classList.toggle('active', angle === current_angle);
      }
    });
  },

  // Initialiser les boutons
  initialize: function() {
    this.updateAngleButtons();

    // Mettre à jour l'état du bouton circulaire
    const circularBtn = document.getElementById('b-circular-mode');
    if (circularBtn && isCircularMode) {
      circularBtn.classList.add('active');
    }
  }
};

// ========== MODULE DE GESTION DE L'OVERFLOW ==========
const ModernOverflowManager = {
  // Configuration
  config: {
    itemHeight: 30,        // Hauteur d'un élément (sync avec CSS --place-height-compact)
    headerHeight: 28,      // Hauteur header overflow
    tolerance: { compact: 6, extended: 3 },
    maxIterations: 5,
    maxSpaceRatio: 0.5     // Maximum 50% de l'écran pour l'overflow
  },

  // État
  originalListHeight: null,
  currentOverflowSections: [],
  isProcessing: false,

  /**
   * Initialise le système d'overflow
   * @returns {boolean} true si l'initialisation réussit
   */
  initialize: function() {
    const list = document.querySelector('.places-list');
    if (!list) return false;

    // Sauvegarder la hauteur originale
    this.originalListHeight = list.clientHeight;
    console.log('📏 Hauteur liste initiale:', this.originalListHeight);

    return true;
  },

  // Gestion de la déduplication
  deduplicateByPlace: function(matchingItems) {
    const seenPlaces = new Set();
    return matchingItems.filter(item => {
      const placeName = item.placeName || item.element?.dataset?.place;
      if (!placeName || seenPlaces.has(placeName)) {
        return false;
      }
      seenPlaces.add(placeName);
      return true;
    });
  },

  /**
   * Algorithme principal de stabilisation de l'overflow
   * @param {Array} matchingItems - Éléments à garder visibles
   */
  stabilizeOverflow: function(matchingItems) {
    const list = document.querySelector('.places-list');
    if (!list) return;

    const currentOverflow = this.calculateOverflowWithConstraints(matchingItems, list.clientHeight);

    if (!currentOverflow.above?.length && !currentOverflow.below?.length) {
      console.log('✅ Aucun overflow détecté');
      return;
    }

    console.log(`📊 Overflow détecté: ${currentOverflow.above?.length || 0} au-dessus, ${currentOverflow.below?.length || 0} en-dessous`);

    // ✅ FIXÉ - ne pas passer de reservedSpace
    this.displayOverflowInReservedSpace(currentOverflow);
  },

  /**
   * Calcule l'espace requis pour afficher l'overflow
   * @param {Object} overflowData - Données d'overflow {above: [], below: []}
   * @returns {number} Espace requis en pixels
   */
  calculateRequiredSpace: function(overflowData) {
    if (!overflowData || (!overflowData.above?.length && !overflowData.below?.length)) {
      return 0;
    }

    let requiredSpace = 0;

    // Espace pour overflow au-dessus
    if (overflowData.above?.length > 0) {
      const count = Math.min(overflowData.above.length, 5); // Max 5 éléments affichés
      requiredSpace += this.config.headerHeight + (count * this.config.itemHeight);
    }

    // Espace pour overflow en-dessous
    if (overflowData.below?.length > 0) {
      const count = Math.min(overflowData.below.length, 5);
      requiredSpace += this.config.headerHeight + (count * this.config.itemHeight);
    }

    // Limiter l'espace maximum
    const maxAllowedSpace = Math.floor(this.originalListHeight * this.config.maxSpaceRatio);
    const finalSpace = Math.min(requiredSpace, maxAllowedSpace);

    console.log(`📐 Espace: ${requiredSpace}px → ${finalSpace}px (max: ${maxAllowedSpace}px)`);
    return finalSpace;
  },

  /**
   * Détecte quels éléments sont en overflow
   * @param {Array} highlightedItems - Éléments à vérifier
   * @param {number} maxHeight - Hauteur maximale de la liste
   * @returns {Object} {above: [], below: []} éléments en overflow
   */
  calculateOverflowWithConstraints: function(highlightedItems, maxHeight) {
    const list = document.querySelector('.places-list');
    if (!list || !highlightedItems.length || maxHeight <= 0) {
      return { above: [], below: [] };
    }

    const scrollTop = list.scrollTop;
    const scrollBottom = scrollTop + maxHeight;

    const overflowAbove = [];
    const overflowBelow = [];

    highlightedItems.forEach(item => {
      const element = item.element || item.place;
      if (!element) return;

      const row = element.closest('.place-row');
      if (!row) return;

      const itemTop = row.offsetTop;
      const itemBottom = itemTop + row.offsetHeight;

      // Simple test de visibilité : l'élément est-il entièrement visible ?
      const isCompletelyVisible = (itemTop >= scrollTop && itemBottom <= scrollBottom);

      if (!isCompletelyVisible) {
        // Déterminer si l'élément est plutôt au-dessus ou en-dessous du viewport
        const itemCenter = (itemTop + itemBottom) / 2;
        const viewCenter = (scrollTop + scrollBottom) / 2;

        if (itemCenter < viewCenter) {
          overflowAbove.push({
            element: element,
            index: item.index,
            row: row,
            placeName: item.placeName
          });
        } else {
          overflowBelow.push({
            element: element,
            index: item.index,
            row: row,
            placeName: item.placeName
          });
        }
      }
    });

    return { above: overflowAbove, below: overflowBelow };
  },

  /**
   * Vérifie si l'overflow est stable entre deux itérations
   * @param {Object} current - Overflow actuel
   * @param {Object} previous - Overflow précédent
   * @returns {boolean} true si stable
   */
  isOverflowStable: function(current, previous) {
    if (!previous) return false;

    const currentCounts = {
      above: current.above?.length || 0,
      below: current.below?.length || 0
    };

    const previousCounts = {
      above: previous.above?.length || 0,
      below: previous.below?.length || 0
    };

    return (currentCounts.above === previousCounts.above) &&
           (currentCounts.below === previousCounts.below);
  },

  /**
   * Détecte une oscillation dans le calcul
   * @param {Object} current - Overflow actuel
   * @param {Object} previous - Overflow précédent
   * @returns {boolean} true si oscillation détectée
   */
  detectOscillation: function(current, previous) {
    if (!previous) return false;

    // Oscillation = les éléments changent de côté
    const currentAboveIds = current.above.map(item => item.index).join(',');
    const currentBelowIds = current.below.map(item => item.index).join(',');
    const previousAboveIds = previous.above.map(item => item.index).join(',');
    const previousBelowIds = previous.below.map(item => item.index).join(',');

    return (currentAboveIds === previousBelowIds) || (currentBelowIds === previousAboveIds);
  },

  /**
   * Affiche l'overflow dans l'espace réservé
   * @param {Object} overflowData - Données d'overflow
   * @param {number} reservedSpace - Espace réservé en pixels
   */
  displayOverflowInReservedSpace: function(overflowData, reservedSpace) {
    this.clearOverflowSections();

    if (!overflowData.above?.length && !overflowData.below?.length) {
      return;
    }

    const container = document.querySelector('.places-container');
    if (!container) return;

    // Overflow au-dessus - positionner en HAUT de la liste
    if (overflowData.above?.length > 0) {
      const aboveSection = this.createOverflowSection('above', overflowData.above);
      aboveSection.style.top = '0px'; // ✅ FIXÉ - en haut !
      container.appendChild(aboveSection);
      this.currentOverflowSections.push(aboveSection);
    }

    // Overflow en-dessous - utiliser les classes CSS (bottom: 0)
    if (overflowData.below?.length > 0) {
      const belowSection = this.createOverflowSection('below', overflowData.below);
      // ✅ FIXÉ - laisser le CSS gérer avec .overflow-section.below
      container.appendChild(belowSection);
      this.currentOverflowSections.push(belowSection);
    }
  },

  /**
   * Crée une section d'overflow
   * @param {string} position - 'above' ou 'below'
   * @param {Array} items - Éléments en overflow
   * @returns {HTMLElement} Section créée
   */
  createOverflowSection: function(position, items) {
    const section = document.createElement('div');
    section.className = `overflow-section ${position} stabilized`;

    // Header - utilise uniquement la classe CSS existante
    const header = document.createElement('div');
    header.className = 'overflow-header';
    header.innerHTML = `
      <i class="fas fa-arrow-${position === 'above' ? 'up' : 'down'} fa-sm"></i>
      ${items.length} lieu${items.length > 1 ? 'x' : ''} hors écran
    `;

    // Contenu - utilise uniquement la classe CSS existante
    const content = document.createElement('div');
    content.className = 'overflow-content';

    // Trier les items selon l'ordre alphabétique au lieu de l'ordre DOM
    const sortedItems = this.sortItemsByLogicalOrder(items);

    items.slice(0, 5).forEach(item => {
      const row = item.row;
      if (row) {
        const clone = row.cloneNode(true);

        // Nettoyer IDs et classes conflictuelles
        const indicators = clone.querySelector('.place-indicators');
        const placeContent = clone.querySelector('.place-content');

        if (indicators) indicators.removeAttribute('id');
        if (placeContent) {
          placeContent.removeAttribute('id');
          placeContent.classList.remove('grayed-out');
        }

        content.appendChild(clone);
      }
    });

    section.appendChild(header);
    section.appendChild(content);
    return section;
  },

  // Trier selon l’ordre logique (parent avant enfant)
  sortItemsByLogicalOrder: function(items) {
    return items.sort((a, b) => {
      const aPlace = a.placeName;
      const bPlace = b.placeName;
      const aData = lieux[aPlace];
      const bData = lieux[bPlace];

      // Si A est parent de B, A avant B
      if (bData?.isSubLocation && bData.parentLocation === aPlace) return -1;
      // Si B est parent de A, B avant A
      if (aData?.isSubLocation && aData.parentLocation === bPlace) return 1;

      // Sinon tri alphabétique normal
      return aPlace.localeCompare(bPlace, 'fr', { sensitivity: 'base' });
    });
  },
};

// ========== Application principale ==========
const FanchartApp = {
  window_w: 0,
  window_h: 0,
  zoom_factor: CONFIG.zoom_factor,

  // Méthodes de ViewManager intégrées
  zoom: function(zx, zy, factor, direction) {
    var w = svg_viewbox_w;
    var h = svg_viewbox_h;
    if (direction > 0) {
      h = Math.round(h/factor);
      w = Math.round(w/factor);
    } else {
      h = Math.round(h*factor);
      w = Math.round(w*factor);
    }
    this.set_svg_viewbox(
      svg_viewbox_x + Math.round(zx * (svg_viewbox_w - w) / this.window_w),
      svg_viewbox_y + Math.round(zy * (svg_viewbox_h - h) / this.window_h),
      w, h
    );
  },

  set_svg_viewbox: function(x, y, w, h) {
    svg_viewbox_x = x;
    svg_viewbox_y = y;
    svg_viewbox_w = w;
    svg_viewbox_h = h;
    fanchart.setAttribute("viewBox", x + " " + y + " " + w + " " + h);
  },

  fitScreen: function() {
    this.set_svg_viewbox(0, 0, svg_w, svg_h);
  },

  processAncestorData: function() {
    // Vue d'ensemble claire : on voit immédiatement les étapes du traitement
    const ancestorKeys = Object.keys(ancestor);

    ancestorKeys.forEach(key => {
      const person = ancestor[key];

      // Chaque transformation a sa propre fonction dédiée
      this.cleanPersonPlaces(person, key);
      this.cleanPersonDates(person, key);
      this.cleanPersonAge(person, key);
    });

    // Après le nettoyage, mettre à jour les flags globaux
    this.updateGlobalFlags();
  },

  // Nettoyage des lieux
  cleanPersonPlaces: function(person, key) {
    Events.types.forEach(eventType => {
      const placeField = Events.place(eventType); // 'birth_place', etc.
      const flagName = Events.flagProp(eventType); // 'has_b', etc.

      if (person[placeField] !== undefined) {
        ancestor[key][placeField] = this.cleanPlaceName(person[placeField]);
        window[flagName] = true;
      }
    });
  },

  // Fonction pure pour nettoyer un nom de lieu
  cleanPlaceName: function(placeName) {
    // Cette fonction est pure : même entrée = même sortie, pas d'effets de bord
    return placeName.replace(/^\?, /, "");
  },

  // Fonction dédiée au nettoyage des dates
  cleanPersonDates: function(person, key) {
    if (person.dates !== undefined) {
      // Chaînage des transformations de manière claire
      let cleanedDates = person.dates;
      cleanedDates = this.removeHtmlTags(cleanedDates);
      cleanedDates = this.abbreviateCirca(cleanedDates);
      ancestor[key].dates = cleanedDates;
    }
  },

  // Fonctions pures pour les transformations de dates
  removeHtmlTags: function(text) {
    return text.replace(/\s?<\/?bdo[^>]*>/g, "");
  },

  abbreviateCirca: function(text) {
    return text.replace(/\bca\s+/g, "~");
  },

  // Fonction dédiée au nettoyage de l'âge
  cleanPersonAge: function(person, key) {
    if (person.death_age !== undefined) {
      ancestor[key].death_age = this.extractNumericAge(person.death_age);
    }
  },

  // Fonction pure pour extraire l'âge numérique
  extractNumericAge: function(ageString) {
    return ageString.replace(/[^0-9]/g, "");
  },

  // Mise à jour des flags globaux basée sur l'état actuel des données
  updateGlobalFlags: function() {
    // Réinitialiser tous les flags via Events
    Events.types.forEach(eventType => {
      const flagName = Events.flagProp(eventType); // 'has_b', 'has_ba', etc.
      window[flagName] = false;
    });

    // Parcourir les ancêtres pour déterminer quels types sont présents
    Object.values(ancestor).forEach(person => {
      Events.types.forEach(eventType => {
        const placeField = Events.place(eventType);
        const flagName = Events.flagProp(eventType);
        if (person[placeField]) {
          window[flagName] = true;
        }
      });
    });
  },

  checkForImplexes: function() {
    for (let key in ancestor) {
      if (ancestor[key].sosasame) {
        return true;
      }
    }
    return false;
  },

  hasParentsInNextGeneration: function() {
    const lastGenStart = Math.pow(2, max_gen);
    const lastGenEnd = Math.pow(2, max_gen + 1) - 1;

    for (let sosa = lastGenStart; sosa <= lastGenEnd; sosa++) {
      const person = ancestor["S" + sosa];
      if (person && person.has_parents) {
        return true;
      }
    }
    return false;
  },

  updateButtonStates: function() {
    // Gestion du bouton implexes
    const implexButton = document.getElementById("b-implex");
    if (implexButton) {
      if (this.checkForImplexes()) {
        implexButton.style.display = "inline-flex";
      } else {
        implexButton.style.display = "none";
      }
    }

    // Gestion intelligente du bouton ajouter génération
    const addButton = document.getElementById("b-gen-add");
    if (addButton) {
      let canAdd = false;
      let hasParentsAvailable = false;

      if (max_gen < max_gen_loaded) {
        canAdd = true;
      } else if (max_gen < 10) {
        hasParentsAvailable = FanchartApp.hasParentsInNextGeneration();
        canAdd = hasParentsAvailable;
      }

      addButton.classList.toggle("disabled", !canAdd);
      addButton.disabled = !canAdd;

      if (max_gen < max_gen_loaded) {
        addButton.title = "Afficher la génération suivante (données en mémoire)";
      } else if (hasParentsAvailable) {
        addButton.title = "Charger la génération suivante";
      } else {
        addButton.title = "Aucun parent dans la génération suivante";
      }
    }
  },

  calculateDimensions: function() {
    // Calculer max_gen depuis ancestor
    var ak = Object.keys(ancestor);
    max_gen_loaded = Math.trunc(Math.log(Number(ak[ak.length-1].replace(/^S/, "")))/Math.log(2));

    if (typeof max_gen === 'undefined') {
      max_gen = max_gen_loaded;
    }
    if (max_gen > max_gen_loaded) {
      max_gen = max_gen_loaded;
      URLManager.updateCurrentURL();
    }

    // Calculer max_r avec validation
    max_r = 0;
    for (var i = 0; i < max_gen+1 && i < CONFIG.a_r.length; i++) {
      max_r += CONFIG.a_r[i];
    }

    if (isNaN(max_r) || max_r <= 0) {
      console.error("max_r invalide:", max_r);
      max_r = 300;
    }

    // Définir les dimensions du SVG avec validation
    const margin = CONFIG.svg_margin;

    if (isCircularMode) {
      // Mode circulaire : forcer un carré pour contenir le cercle complet
      const size = 2 * (max_r + margin);
      svg_w = size;
      svg_h = size;
      center_x = size / 2;
      center_y = size / 2;
    } else {
      // Calcul standard pour tous les angles
      center_x = max_r + margin;
      center_y = max_r + margin;
      svg_w = 2 * center_x;

      if (current_angle === 180) {
        // Demi-cercle : hauteur réduite, le centre est positionné en haut
        // MAIS le cercle S1 dépasse vers le bas de CONFIG.a_r[0]
        svg_h = max_r + CONFIG.a_r[0] + 2 * margin; // Ajouter le rayon du cercle central
      } else if (current_angle <= 270) {
        center_y = max_r + margin;
        // Angles standard : calcul avec extension
        const halfAngleRad = Math.PI/180 * (current_angle - 180) / 2;
        const extraHeight = Math.max(CONFIG.a_r[0], Math.round(max_r * Math.sin(halfAngleRad)));
        svg_h = 2 * margin + max_r + extraHeight;
      } else {
        // Angles > 270° : hauteur complète nécessaire
        svg_h = 2 * (max_r + margin);
      }
    }

    if (isNaN(svg_w) || isNaN(svg_h) || svg_w <= 0 || svg_h <= 0) {
      console.error("Dimensions SVG calculées invalides:", { svg_w, svg_h, max_r, center_x, center_y });
      svg_w = 800;
      svg_h = 600;
      center_x = svg_w / 2;
      center_y = svg_h / 2;
    }

    // Dimensions de la fenêtre avec limitation pour la liste des lieux

    this.window_h = window.innerHeight;
    const maxAllowedWidth = window.innerWidth * 0.85; // 85% max de la largeur
    const calculatedWidth = Math.round(this.window_h * svg_w / svg_h);
    this.window_w = Math.min(calculatedWidth, maxAllowedWidth);

    // Si on a dû réduire la largeur, ajuster la hauteur en conséquence
    if (this.window_w < calculatedWidth) {
      this.window_h = Math.round(this.window_w * svg_h / svg_w);
    }

    // Ajuster la position pour les modes carrés (éviter l'empiètement)
    if (current_angle >= 310 || isCircularMode) {
      fanchart.style.marginLeft = '120px';
    } else {
      fanchart.style.marginLeft = '0';
    }

    // Configurer le SVG
    fanchart.setAttribute("height", this.window_h);
    fanchart.setAttribute("width", this.window_w);

    // Mettre à jour la variable CSS pour la largeur de la liste
     const actualListWidth = LayoutCalculator.calculatePlacesListWidth();
     root.style.setProperty('--fc-tool-size', actualListWidth + 'px');

    // Initialisation de la viewbox
    svg_viewbox_x = 0;
    svg_viewbox_y = 0;
    svg_viewbox_w = svg_w;
    svg_viewbox_h = svg_h;
  },

  renderFanchart: function() {
    // Initialiser le texte standard
    const standardInfo = this.initializeStandardText();
    standard = standardInfo.element;
    standard_width = standardInfo.width;

    if (isCircularMode) {
      // Nettoyer complètement le SVG
      while (fanchart.firstChild) {
        fanchart.removeChild(fanchart.firstChild);
      }

      // Réinitialiser après nettoyage
      const newStandardInfo = this.initializeStandardText();
      standard = newStandardInfo.element;
      standard_width = newStandardInfo.width;

      // Sauvegarder l'état
      const savedAngle = current_angle;
      const originalAncestors = ancestor;

      // Forcer l'angle à 180° pour les demi-cercles
      current_angle = 180;

      // Créer les groupes pour les deux hémisphères
      const northGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
      northGroup.setAttribute("id", "north-hemisphere");
      fanchart.appendChild(northGroup);

      const southGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
      southGroup.setAttribute("id", "south-hemisphere");
      southGroup.setAttribute("transform", `rotate(180 ${center_x} ${center_y})`);
      fanchart.appendChild(southGroup);

      // RENDU NORD : Lignée paternelle (S2)
      ancestor = CircularModeRenderer.shiftAncestorsForParent(originalAncestors, 2);
      renderTarget = northGroup; // CRITIQUE : rediriger TOUS les rendus vers le groupe

      // Pas de centre S1 pour les demi-disques
      this.renderAncestorsByGeneration();

      // RENDU SUD : Lignée maternelle (S3)
      ancestor = CircularModeRenderer.shiftAncestorsForParent(originalAncestors, 3);
      renderTarget = southGroup;

      this.renderAncestorsByGeneration();

      // Restaurer l'état
      renderTarget = null;
      ancestor = originalAncestors;
      current_angle = savedAngle;

      // Ajouter le centre en mode couple
      CircularModeRenderer.renderCoupleCenter();

    } else {
      // Mode éventail normal
      this.renderCenterPerson();
      this.renderAncestorsByGeneration();
    }

    this.updateButtonStates();
  },

  renderAncestorsByGeneration: function() {
    // rayon total accumulé
    let cumulativeR = CONFIG.a_r[0];
    const rings = max_gen + 1;

    for (let gen = 2; gen <= rings; gen++) {
      const innerR = cumulativeR;                // rayon intérieur
      const outerR = innerR + CONFIG.a_r[gen-1]; // rayon extérieur
      cumulativeR = outerR;                      // pour la génération suivante

      // angle total à découper - utilise l'angle dynamique
      const delta = current_angle / Math.pow(2, gen-1);
      // angle de départ au-dessus du centre
      let angle = -90 - current_angle / 2 + delta/2;

      // on itère sur les 2^(gen-1) cases de cette génération
      const firstSosa = Math.pow(2, gen-1);
      const lastSosa  = Math.pow(2, gen) - 1;

      for (let sosa = firstSosa; sosa <= lastSosa; sosa++, angle += delta) {
        const person = this.getEffectivePerson(sosa);
        if (!person) continue;

        // prépare la position de ce secteur
        const pos = {
          r1: innerR, r2: outerR,
          a1: angle - delta/2, a2: angle + delta/2,
          generation: gen, delta: delta
        };

        this.renderAncestorSector(sosa, pos, person);
      }
    }

    this.updateButtonStates();
  },

  initializeAngleEvents: function() {
    CONFIG.available_angles.forEach(angle => {
      const btn = document.getElementById(`b-angle-${angle}`);
      if (btn) {
        btn.onclick = function() {
          AngleManager.setAngle(angle);
        };
      }
    });
  },

  // Résoudre les implexes virtuellement
  getEffectivePerson: function(sosa) {
    let person = ancestor["S" + sosa];
    // Si pas de personne à ce sosa, chercher si c’est un enfant d’implexe
    if (!person && implexMode !== "reduced") {
      const parentSosa = Math.floor(sosa / 2);
      const parentPerson = ancestor["S" + parentSosa];

      if (parentPerson && parentPerson.sosasame) {
        const refSosa = parentPerson.sosasame;
        const childSosa = sosa % 2 === 0 ? 2 * refSosa : 2 * refSosa + 1;
        const childPerson = ancestor["S" + childSosa];

        if (childPerson) {
          // Créer un enfant virtuel basé sur l'implexe
          return {
            ...childPerson,
            fn: implexMode === "numbered" ? "" : childPerson.fn,
            sn: implexMode === "numbered" ? `${sosa} › ${childSosa}` : childPerson.sn,
            sosasame: implexMode === "numbered" ? childSosa : undefined,
            dates: implexMode === "numbered" ? "" : childPerson.dates
          };
        }
      }
    }
    return person;
  },

  initializeStandardText: function() {
    const target = renderTarget || fanchart;
    const standard = document.createElementNS("http://www.w3.org/2000/svg", "text");
    standard.textContent = "ABCDEFGHIJKLMNOPQRSTUVW abcdefghijklmnopqrstuvwxyz";
    standard.setAttribute("id", "standard");
    standard.setAttribute("x", center_x);
    standard.setAttribute("y", center_y);
    target.append(standard);

    const bbox = standard.getBBox();
    return {
      element: standard,
      width: bbox.width / standard.textContent.length,
      height: bbox.height  // Si besoin plus tard
    };
  },

  // Rendu du centre (Sosa 1)
  renderCenterPerson: function() {
    const sosa = 1;
    const person = ancestor["S" + sosa];
    const r = CONFIG.a_r[0];

    // Créer le groupe SVG
    const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
    group.setAttribute("id", "S" + sosa);
    fanchart.append(group);

    // Dessiner les éléments
    SVGRenderer.drawCircle(group, r, center_x, center_y, person, { isBackground: true });
    T.drawText(group, 'S1', {
      x: center_x,
      y: center_y - 10,
      p: person,
      classes: ""
    });
    SVGRenderer.drawCircle(group, r, center_x, center_y, person);

    return group;
  },

  // Gestion des implexes
  handleImplex: function(sosa, person) {
    if (!person.sosasame) {
      return { person: person, isImplex: false };
    }

    const referenceSosa = person.sosasame;
    const referencedPerson = ancestor["S" + referenceSosa];


    if (implexMode === "reduced") {
      // Mode initial (réduit) : garder les " < " et pas de propagation
      return { person: person, isImplex: false };
    }

    if (implexMode === "numbered") {
     // Mode numéroté : utiliser les vraies données mais avec numérotation
      return {
        person: {
          ...referencedPerson,  // Utiliser les vraies données pour les couleurs
          fn: "",               // Mais remplacer le prénom
          sn: `${sosa} › ${referenceSosa}`,  // Et le nom par la numérotation
          dates: "",            // Et les dates
          sosasame: referenceSosa
        },
        isImplex: true,
        originalSosa: referenceSosa
      };
    }

    // Mode "full" : remplacer par les vraies données
    return { person: referencedPerson, isImplex: false };
  },

  // Rendre un secteur complet d’ancêtre
  renderAncestorSector: function(sosa, position, person) {
      const target = renderTarget || fanchart;
      // Créer le groupe pour cet ancêtre
      const group = document.createElementNS("http://www.w3.org/2000/svg", "g");
      group.setAttribute("id", "S" + sosa);
      target.append(group);

      // Gérer les implexes
      const implexInfo = this.handleImplex(sosa, person);
      const actualPerson = implexInfo.person;
      actualPerson.sosa = sosa;

      // Dessiner le secteur de fond
      SVGRenderer.drawPie(group, position.r1 + 10, position.r2,
        position.a1, position.a2, actualPerson,
        { type: 'person', isBackground: true });

      // Dessiner le texte si la personne est connue
      if (actualPerson.fn !== "?") {
        const textClasses = this.buildTextClasses(actualPerson);
        SVGRenderer.drawSectorText(group, position.r1, position.r2,
          position.a1, position.a2, sosa, actualPerson,
          textClasses, position.generation, implexInfo.isImplex);
      }

      // Gérer le mariage pour les ancêtres pairs (pères) - AVANT le secteur interactif
      if (sosa % 2 === 0) {
        const marriageGroup = document.createElementNS("http://www.w3.org/2000/svg", "g");
        marriageGroup.setAttribute("id", "M" + sosa);
        group.appendChild(marriageGroup);  // Ajouter au groupe de la personne, pas au fanchart

        let marriagePerson = actualPerson;
        if (implexInfo.isImplex && implexMode === "numbered" && implexInfo.originalSosa) {
          marriagePerson = ancestor["S" + implexInfo.originalSosa];
        }

        this.renderMarriageInfo(marriageGroup, sosa, position, marriagePerson);
      } else if (sosa % 2 !== 0) {
        // Pour les mères, propager les infos de mariage
        const fatherSosa = sosa - 1;
        const father = this.getEffectivePerson(fatherSosa);

        if (father && father.marriage_place) {
          actualPerson.marriage_place = father.marriage_place;
        }
      }

      // Dessiner le secteur interactif EN DERNIER pour qu'il soit au-dessus
      SVGRenderer.drawPie(group, position.r1 + 10, position.r2,
        position.a1, position.a2, actualPerson,
        { type: 'person' });

      // Ajouter l'indicateur de navigation
      SVGRenderer.drawParentIndicator(group, position.r1 + 10,
        position.a1, position.a2, sosa, actualPerson);

      return group;
  },

  buildTextClasses: function(person) {
    let classes = "";

    if (person.birth_place && person.birth_place !== "" && lieux[person.birth_place]) {
      classes += " bi-t" + lieux[person.birth_place].c;
    }
    if (person.baptism_place && person.baptism_place !== "" && lieux[person.baptism_place]) {
      classes += " ba-t" + lieux[person.baptism_place].c;
    }
    if (person.death_place && person.death_place !== "" && lieux[person.death_place]) {
      classes += " de-t" + lieux[person.death_place].c;
    }
    if (person.burial_place && person.burial_place !== "" && lieux[person.burial_place]) {
      classes += " bu-t" + lieux[person.burial_place].c;
    }

    return classes.trim();
  },

  // Informations de mariage
  renderMarriageInfo: function(marriageGroup, sosa, position, person) {
    const extendedA2 = position.a2 + position.delta;

    // Dessiner le secteur de mariage (fond)
    SVGRenderer.drawPie(marriageGroup, position.r1, position.r1 + 10,
    position.a1, extendedA2, person,
    { type: 'marriage', isBackground: true });

    // Dessiner la date de mariage si elle existe
    if (person.marriage_date !== undefined) {
      let classes = "";
      if (person.marriage_place && person.marriage_place !== "" && lieux[person.marriage_place]) {
        classes += " ma-t" + lieux[person.marriage_place].c;
      }
      T.drawMarriageDate(marriageGroup, sosa, position.r1 + 5,
        position.a1, extendedA2, person.marriage_date, classes);
    }

    // Dessiner le contour et la ligne radiale
    SVGRenderer.drawContour(marriageGroup, position.r1, position.r2, position.a1, extendedA2);
    SVGRenderer.drawRadialLine(marriageGroup, position.r1 + 10, position.r2, position.a2);

    // Dessiner le secteur de mariage interactif
    SVGRenderer.drawPie(marriageGroup, position.r1, position.r1 + 10,
    position.a1, extendedA2, person,
    { type: 'marriage' });
  },

  updateGenerationTitle: function() {
    const genTitle = document.getElementById('generation-section-title');
    if (genTitle) {
      const genLabel = window.t(max_gen > 1 ? 'generations' : 'generation',
                               max_gen > 1 ? 'générations' : 'génération');
      genTitle.textContent = `${max_gen} ${genLabel}`;
    }
  },

  reRenderWithCurrentGenerations: function() {
    // Nettoyer le cache si on augmente les générations
    if (max_gen > LocationDataBuilder._generationCache.size) {
      LocationDataBuilder.clearCache();
    }

    DOMCache.invalidate(); // Invalider les caches DOM
    this.calculateDimensions();
    LocationDataBuilder.buildCompleteLocationData(max_gen);
    const fanchart = document.getElementById("fanchart");
    fanchart.innerHTML = "";
    this.renderFanchart();
    const placesPanel = document.querySelector('.places-panel');
    if (placesPanel && PlacesInterface.cache.elements.panel) {
      // Invalider le cache de tri car les données ont changé
      PlacesInterface.cache.invalidateSort();

      PlacesInterface.generatePlacesList();
      PlacesInterface.updateSummarySection();
    }
    this.fitScreen();
    this.updateButtonStates();
    this.updateGenerationTitle();
  },

  initializeEvents: function() {
    // Zoom
    fanchart.addEventListener("wheel", (event) => {
      this.zoom(event.clientX, event.clientY, CONFIG.zoom_factor,
      (event.deltaY < 0 ? +1 : -1));
    }, { passive: false });

    // Drag
    var drag_state = false;
    fanchart.onmousedown = function(e) {
      e.preventDefault();
      drag_state = true;
    };
    fanchart.onmouseup = () => drag_state = false;
    fanchart.onmousemove = (e) => {
      if (drag_state) {
        e.preventDefault();
        this.set_svg_viewbox(
          svg_viewbox_x - Math.round(e.movementX * svg_viewbox_w / this.window_w),
          svg_viewbox_y - Math.round(e.movementY * svg_viewbox_h / this.window_h),
          svg_viewbox_w, svg_viewbox_h
        );
      }
    };

    // Boutons de navigation
    document.getElementById("b-no-buttons").onclick = function() {
      document.getElementById("fanchart-controls").style.display = "none";
    };
    document.getElementById("b-help").onclick = function() {
      const helpPanel = document.getElementById('navigation-help');
      if (helpPanel) {
        const isVisible = helpPanel.style.display !== 'none';
        helpPanel.style.display = isVisible ? 'none' : 'block';
        this.classList.toggle("active", !isVisible);
      }
    };
    document.getElementById("b-home").onclick = () => {
      window.location = link_to_person;
    };
    document.getElementById("b-rng").onclick = function() {
      const url = this.getAttribute("data-url");
      if (url) { window.location = url; }
    };
    document.getElementById("b-refresh").onclick = () => {
      this.fitScreen();
    };
    document.getElementById("b-zoom-in").onclick = () => {
      this.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, +1);
    };
    document.getElementById("b-zoom-out").onclick = () => {
      this.zoom(this.window_w / 2, this.window_h / 2, this.zoom_factor, -1);
    };
    document.getElementById("b-gen-add").onclick = () => {
      if (this.disabled) return;
      if (max_gen < max_gen_loaded) {
        max_gen++;
        FanchartApp.reRenderWithCurrentGenerations();
        URLManager.updateCurrentURL();
      } else {
        URLManager.navigateToGeneration(max_gen + 1);
      }
    };
    document.getElementById("b-gen-del").onclick = () => {
      if(max_gen > 1) {
        max_gen--;
        FanchartApp.reRenderWithCurrentGenerations();
        URLManager.updateCurrentURL();
      }
    };
    document.getElementById("b-implex").onclick = function() {
      // Cycle : reduced → numbered → full → reduced
      switch(implexMode) {
        case "reduced":
          implexMode = "numbered";
          this.title = "Afficher tous les ancêtres";
          this.querySelector("i").className = "fa fa-comment fa-fw";
          break;
        case "numbered":
          implexMode = "full";
          this.title = "Réduire les implexes";
          this.querySelector("i").className = "fa fa-comment-slash fa-fw";
          break;
        case "full":
          implexMode = "reduced";
          this.title = "Numéroter les implexes";
          this.querySelector("i").className = "fa fa-comment-dots fa-fw";
          break;
      }

      FanchartApp.reRenderWithCurrentGenerations();
      URLManager.updateCurrentURL();
    };
    document.getElementById("font-selector").onchange = function() {
      const fanchart = document.getElementById("fanchart");

      ['mono', 'serif', 'large', 'readable'].forEach(cls => {
        fanchart.classList.remove(cls);
      });

      if (this.value) {
        fanchart.classList.add(this.value);
      }

      // Mettre à jour l'URL si nécessaire
      // URLManager.updateCurrentURL();
    };

  },

  applyInitialState: function() {
    // Configurer l'état initial des outils
    if (tool == "death-age") {
      document.body.className = "death-age";
      const ageButton = document.getElementById("b-death-age");
      if (ageButton) ageButton.classList.add("active");
    } else {
      document.body.className = "place_color";
      tool = "place_color";
      const placesButton = document.getElementById("b-places-colorise");
      if (placesButton) placesButton.classList.add("active");

      const maCheckbox = document.getElementById("ma");
      if (maCheckbox) maCheckbox.checked = true;
      ColorManager.applyColorization();
    }

    if (typeof HighlightCoordinator !== 'undefined' && HighlightCoordinator.config) {
      HighlightCoordinator.config.enableBidirectional = document.body.classList.contains('place_color');
      console.log('✅ enableBidirectional forcé à:', HighlightCoordinator.config.enableBidirectional);
    }
  },
  
  init: function() {
    console.log('🚀 Initialisation du FanChart...');
    
    // ========== PHASE 1 : CALCULS ET DIMENSIONS ==========
    console.log('📐 Phase 1 : Calculs et dimensions');
    this.calculateDimensions();
    
    // ========== PHASE 2 : TRAITEMENT DES DONNÉES ==========
    console.log('🔄 Phase 2 : Traitement des données ancêtres');
    this.processAncestorData();
    
    // ========== PHASE 3 : LECTURE DE L'ÉTAT URL ==========
    console.log('🔗 Phase 3 : Lecture de l\'état URL');
    const state = URLManager.readCurrentState();
    tool = state.tool;
    sortMode = state.sortMode;
    showEvents = state.showEvents;
    isCircularMode = state.isCircular;
    current_angle = state.angle;
    implexMode = state.implexMode;
    
    // ========== PHASE 4 : CONSTRUCTION DES DONNÉES DE LIEUX ==========
    console.log('🏗️ Phase 4 : Construction des données de lieux');
    LocationDataBuilder.buildCompleteLocationData();
    
    // ========== PHASE 5 : GÉNÉRATION DES STYLES CSS ==========
    console.log('🎨 Phase 5 : Génération des styles CSS');
    // Créer d'abord les classes de colorisation des lieux
    let colorIndex = 0;
    Object.entries(lieux).forEach(([placeName, data]) => {
      const hue = (colorIndex * 360 / Object.keys(lieux).length) % 360;
      const lightness = 65;
      ColorManager.createLocationStyles(colorIndex, hue, lightness);
      colorIndex++;
    });
    
    // Si la fonction HighlightCSSGenerator existe, l'utiliser aussi
    if (typeof HighlightCSSGenerator !== 'undefined' && HighlightCSSGenerator.generateLocationStyles) {
      HighlightCSSGenerator.generateLocationStyles();
    }
    
    // ========== PHASE 6 : INTERFACE DES LIEUX ==========
    console.log('📋 Phase 6 : Interface des lieux');
    if (document.querySelector('.places-panel')) {
      PlacesInterface.initialize();
      PlacesPanelControls.initialize();
      
      // Enrichir les données pour le système de surlignage
      if (PlacesInterface.exposeDataToHighlightSystem) {
        PlacesInterface.exposeDataToHighlightSystem();
      }
    }
    
    // ========== PHASE 7 : APPLICATION DE L'ÉTAT INITIAL ==========
    console.log('⚙️ Phase 7 : Application de l\'état initial');
    this.applyInitialState();
    
    // ========== PHASE 8 : RENDU DU GRAPHIQUE ==========
    console.log('🎯 Phase 8 : Rendu du graphique SVG');
    this.renderFanchart();
    
    // ========== PHASE 9 : MISE À JOUR DES CONTRÔLES ==========
    console.log('🎮 Phase 9 : Mise à jour des contrôles');
    
    // Boutons d'angle
    AngleManager.updateAngleButtons();
    
    // Bouton circulaire
    if (isCircularMode) {
      const circularBtn = document.getElementById('b-circular-mode');
      if (circularBtn) circularBtn.classList.add('active');
    }
    
    // Mise à jour des états des boutons (implexes, générations)
    this.updateButtonStates();
    
    // ========== PHASE 10 : INITIALISATION DES ÉVÉNEMENTS ==========
    console.log('🖱️ Phase 10 : Initialisation des événements');
    
    // Préchargement du cache DOM
    if (DOMCache && DOMCache.preload) {
      DOMCache.preload();
    }
    
    // Événements de base (zoom, drag, navigation)
    this.initializeEvents();
    
    // Événements des angles
    if (this.initializeAngleEvents) {
      this.initializeAngleEvents();
    }
    
    // Événements de colorisation
    if (ColorManager && ColorManager.initializeColorEvents) {
      ColorManager.initializeColorEvents();
    }
    
    // ========== PHASE 11 : SYSTÈME DE SURLIGNAGE ==========
    console.log('✨ Phase 11 : Système de surlignage');
    
    // Délai pour s'assurer que le DOM est complètement prêt
    setTimeout(() => {
      // Vérifier que les modules sont disponibles
      if (typeof IntegrationHelper !== 'undefined') {
        // Initialiser le système principal
        if (IntegrationHelper.initializeHighlightSystem) {
          IntegrationHelper.initializeHighlightSystem();
        }
        
        // Initialiser les références
        if (IntegrationHelper.initializeHighlightReferences) {
          IntegrationHelper.initializeHighlightReferences();
        }
      }
      
      // Forcer la mise à jour de enableBidirectional
      if (typeof HighlightCoordinator !== 'undefined' && HighlightCoordinator.config) {
        const isPlaceColorActive = document.body.classList.contains('place_color');
        HighlightCoordinator.config.enableBidirectional = isPlaceColorActive;
        console.log('🔧 enableBidirectional forcé à:', isPlaceColorActive);
      }
      
      // Vérification optionnelle des classes en mode debug
      if (window.location.search.includes('debug=highlight')) {
        setTimeout(() => {
          if (window.verifyClassesOnElements) {
            window.verifyClassesOnElements();
          }
        }, 500);
      }
      
    }, 300); // 300ms de délai pour la stabilité
    
    // ========== PHASE 12 : FINALISATION ==========
    console.log('🏁 Phase 12 : Finalisation');
    
    // Titre des générations
    this.updateGenerationTitle();
    
    // Panneau d'aide à la navigation
    if (UIManager && UIManager.addNavigationHelp) {
      UIManager.addNavigationHelp();
    }
    
    // Ajustement final de la vue
    this.fitScreen();
    
    console.log('✅ Initialisation du FanChart terminée');
    // Ajouter ?autotest=highlight dans l'URL pour lancer les tests automatiquement
    if (window.location.search.includes('autotest=highlight')) {
      setTimeout(() => {
        console.log('🧪 Lancement des tests automatiques...');
        if (window.testHighlightInteractions) {
          window.testHighlightInteractions();
        }
      }, 1000);
    }
  }
};

window.debugInitSequence = function() {
  console.group('🔍 Vérification de la Séquence d\'Initialisation');
  
  console.log('1. Dimensions calculées:', {
    svg_w: typeof svg_w !== 'undefined' ? svg_w : 'NON DÉFINI',
    svg_h: typeof svg_h !== 'undefined' ? svg_h : 'NON DÉFINI',
    center_x: typeof center_x !== 'undefined' ? center_x : 'NON DÉFINI',
    center_y: typeof center_y !== 'undefined' ? center_y : 'NON DÉFINI'
  });
  
  console.log('2. État URL lu:', {
    tool: tool,
    sortMode: sortMode,
    showEvents: showEvents,
    isCircularMode: isCircularMode,
    current_angle: current_angle
  });
  
  console.log('3. Données construites:', {
    lieux: typeof lieux === 'object' ? Object.keys(lieux).length + ' lieux' : 'NON DÉFINI',
    ancestor: typeof ancestor === 'object' ? Object.keys(ancestor).length + ' ancêtres' : 'NON DÉFINI'
  });
  
  console.log('4. État du body:', {
    classes: document.body.className,
    place_color_actif: document.body.classList.contains('place_color')
  });
  
  console.log('5. Système de surlignage:', {
    HighlightCoordinator: typeof HighlightCoordinator !== 'undefined' ? 'CHARGÉ' : 'NON CHARGÉ',
    enableBidirectional: typeof HighlightCoordinator !== 'undefined' ? 
      HighlightCoordinator.config.enableBidirectional : 'N/A'
  });
  
  console.log('6. Éléments SVG avec classes:', {
    'path.link avec classes lieu': document.querySelectorAll('path.link[class*="-L"]').length,
    'circle.link avec classes lieu': document.querySelectorAll('circle.link[class*="-L"]').length
  });
  
  console.groupEnd();
};

window.verifyClassesOnElements = function() {
  console.group('🔍 Vérification des Classes après Correction');
  
  // Vérifier les éléments .link
  const linkElements = document.querySelectorAll('path.link, circle.link');
  console.log(`Nombre d'éléments .link: ${linkElements.length}`);
  
  // Compter les éléments avec classes de lieu
  let withPlaceClasses = 0;
  linkElements.forEach(el => {
    const hasPlaceClass = Events.types.some(type => {
      const prefix = Events.svgPrefix(type);
      return el.className.baseVal.includes(`${prefix}-L`);
    });
    if (hasPlaceClass) withPlaceClasses++;
  });
  
  console.log(`Éléments .link avec classes de lieu: ${withPlaceClasses}`);
  
  // Exemples spécifiques
  Events.types.forEach(type => {
    const prefix = Events.svgPrefix(type);
    const count = document.querySelectorAll(`path.link[class*="${prefix}-L"], circle.link[class*="${prefix}-L"]`).length;
    console.log(`  ${type} (.link.${prefix}-L*): ${count} éléments`);
  });
  
  // Vérifier enableBidirectional
  if (typeof HighlightCoordinator !== 'undefined') {
    console.log('\nenableBidirectional:', HighlightCoordinator.config.enableBidirectional);
  }
  
  console.groupEnd();
};


// Alias pour la rétrocompatibilité
const R = SVGRenderer;
const T = TextRenderer;

window.toggleFanchartSort = () => PlacesPanelControls.toggleSort();
window.toggleFanchartEventsDisplay = () => PlacesPanelControls.toggleEventsDisplay();
window.filterFanchartPlaces = (query) => PlacesPanelControls.filterPlaces(query);
window.clearFanchartSearch = () => PlacesPanelControls.clearSearch();
window.toggleFanchartPlacesPanel = () => document.body.classList.toggle('place_color');

// Lancement de l'application
FanchartApp.init();