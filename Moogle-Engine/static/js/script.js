document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search-input');
    const fileList = document.getElementById('file-list');

    function loadFiles(query = '') {
        if (!query) {
            // Si no hay consulta, mostrar todos los archivos
            fetch('/files')
                .then(response => response.json())
                .then(files => displayFiles(files))
                .catch(error => console.error("Error:", error));
        } else {
            // Si hay consulta, usar la búsqueda TF-IDF
            const startTime = performance.now();
            fetch(`/search?query=${encodeURIComponent(query)}`, {
                method: 'GET',
                headers: { 'Content-Type': 'application/json' },
                params: { query: query }
            })
                .then(response => response.json())
                .then(results => {
                    fileList.innerHTML = '';
                    if (Boolean(results[0].suggestion)) {
                        const endTime = performance.now();
                        const tiempoTranscurrido = ((endTime - startTime) / 1000).toFixed(5);    
                        document.getElementById('time-result').innerText = "La búsqueda demoró: " + tiempoTranscurrido + " segs";
                        document.querySelector('.time').style.display = 'block';
                        document.getElementById('not-found-result').innerText = `No se encontraron resultados para "${query}"`;
                        document.querySelector('.not-found').style.display = 'block';
                       
                        let suggestions = ""
                        const displaySuggestion = ({ suggestion, _ }) => {
                            console.log(suggestion);
                            suggestions += `${suggestion} `;
                        };
                        
                        results
                        .map(result => ({
                            suggestion: result.suggestion,
                            distance: result.distance
                        }))
                        .forEach(displaySuggestion);

                        document.getElementById('suggestion-result').innerText = `¿Quisiste decir "${suggestions}"?`;
                        document.querySelector('.suggestion').style.display = 'block';                          
                    } else {
                        document.querySelector('.not-found').style.display = 'none';
                        document.querySelector('.suggestion').style.display = 'none';

                        const createListItem = (doc, sim) => {
                            const simText = sim !== undefined && sim !== null ? sim : 'N/A';
                            const li = document.createElement('li');
                            
                            // Crear contenedor principal
                            const mainContent = document.createElement('div');
                            mainContent.className = 'main-content';
                            mainContent.innerHTML = `
                                <div class="file-info">
                                    <span>${doc}</span>
                                    <a href="/download/${doc}" class="download-link" onclick="event.stopPropagation()">Descargar</a>
                                    <span>(Relevancia: ${simText})</span>
                                </div>
                                <span class="arrow">▼</span>
                            `;

                            // Crear contenedor para el snippet
                            const snippetContainer = document.createElement('div');
                            snippetContainer.className = 'snippet-container';    
                            li.appendChild(mainContent);
                            li.appendChild(snippetContainer);
                            const arrow = mainContent.querySelector('.arrow');
                            li.addEventListener('click', async () => {
                                const isActive = snippetContainer.classList.contains('active');    
                                if (!isActive) {
                                    try {
                                        const response = await fetch(`/snippet/${encodeURIComponent(doc)}?query=${encodeURIComponent(searchInput.value)}`);
                                        if (!response.ok) {
                                            throw new Error(`HTTP error! status: ${response.status}`);
                                        }
                                        const data = await response.json();
                                        
                                        if (data.error) {
                                            throw new Error(data.error);
                                        }
                                        
                                        // Decodificar el snippet correctamente y limpiar caracteres inválidos
                                        let snippet = data.snippet
                                            .replace(/[\x00-\x09\x0B-\x0C\x0E-\x1F\x7F-\x9F]/g, '') // Eliminar caracteres de control
                                            .replace(/�/g, ''); // Eliminar caracteres de reemplazo inválidos
                                        
                                        searchInput.value.split(' ').forEach(term => {
                                            if (term.trim()) {  // Solo procesar términos no vacíos
                                                const regex = new RegExp(term, 'gi');
                                                snippet = snippet.replace(regex, match => `<span class="highlight">${match}</span>`);
                                            }
                                        });
                                        
                                        snippetContainer.innerHTML = `<div class="snippet-text">${snippet}</div>`;
                                    } catch (error) {
                                        console.error('Error al cargar el snippet:', error);
                                        snippetContainer.innerHTML = `<div class="snippet-text">Error: ${error.message}</div>`;
                                    }
                                }
                                snippetContainer.classList.toggle('active');
                                arrow.classList.toggle('active');
                            });

                            return li;
                        };
                        const appendResultsToFileList = (results, fileList) => {
                            const listItems = results.map(result => 
                                createListItem(result.document, result.similarity)
                            );
                            listItems.forEach(li => fileList.appendChild(li));
                        };
                        appendResultsToFileList(results, fileList);
                    }
                    const endTime = performance.now();
                    const tiempoTranscurrido = ((endTime - startTime) / 1000).toFixed(5);
                    document.getElementById('time-result').innerText = "La búsqueda demoró: " + tiempoTranscurrido + " segs";
                    document.querySelector('.time').style.display = 'block';
                })
                .catch(error => console.error("Error:", error));
        }
    }

    const showFile = (file) => {
        const li = document.createElement('li');
        li.innerHTML = `
            <span>${file}</span>
            <a href="/download/${file}" class="download-link">Descargar</a>
        `;
        fileList.appendChild(li);
    }

    function displayFiles(files) {
        document.querySelector('.time').style.display = 'none';
        fileList.innerHTML = '';
        if (files.length === 0) {
            const li = document.createElement('li');
            li.innerHTML = '<span>No hay archivos disponibles</span>';
            fileList.appendChild(li);
        } else {
            files.forEach(showFile);
        }
    }
    // Cargar archivos inicialmente
    loadFiles();
    // Manejar búsqueda
    searchInput.addEventListener('keypress', function(event) {
        if (event.key === 'Enter') {
            loadFiles(searchInput.value);
        }
    });
});
