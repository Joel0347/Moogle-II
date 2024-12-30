document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search-input');
    const fileList = document.getElementById('file-list');
    console.log(fileList);

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
                    if (results.length === 0) {
                        const li = document.createElement('li');
                        li.innerHTML = `<span>No se encontraron resultados para "${query}"</span>`;
                        fileList.appendChild(li);
                    } else {
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
                })
                .catch(error => console.error("Error:", error));
        }
    }

    function displayFiles(files) {
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
