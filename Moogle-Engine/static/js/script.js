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
                        results.forEach(result => {
                            const suggestion = result.suggestion;
                            const distance = result.distance;
                            document.getElementById('suggestion-result').innerText = `¿Quisiste decir "${suggestion}"? (Distance: ${distance})`;
                            document.querySelector('.suggestion').style.display = 'block';
                        });
                    } else {
                        document.querySelector('.not-found').style.display = 'none';
                        document.querySelector('.suggestion').style.display = 'none';
                        results.forEach(result => {
                            const li = document.createElement('li');
                            const doc = result.document;
                            const sim = result.similarity;
                            let simText = 'N/A';
                            if (typeof sim !== 'undefined' && sim !== null) {
                                simText = sim;
                            }
                            li.innerHTML = `
                                <span>${doc} (Relevancia: ${simText})</span>
                                <a href="/download/${doc}" class="download-link">Descargar</a>
                            `;
                            fileList.appendChild(li);
                        });
                    }
                    const endTime = performance.now();
                    const tiempoTranscurrido = ((endTime - startTime) / 1000).toFixed(5);
                    document.getElementById('time-result').innerText = "La búsqueda demoró: " + tiempoTranscurrido + " segs";
                    document.querySelector('.time').style.display = 'block';
                })
                .catch(error => console.error("Error:", error));
        }
    }

    function displayFiles(files) {
        document.querySelector('.time').style.display = 'none';
        fileList.innerHTML = '';
        if (files.length === 0) {
            const li = document.createElement('li');
            li.innerHTML = '<span>No hay archivos disponibles</span>';
            fileList.appendChild(li);
        } else {
            files.forEach(file => {
                const li = document.createElement('li');
                li.innerHTML = `
                    <span>${file}</span>
                    <a href="/download/${file}" class="download-link">Descargar</a>
                `;
                fileList.appendChild(li);
            });
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
